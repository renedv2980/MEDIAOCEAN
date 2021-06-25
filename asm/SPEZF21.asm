*          DATA SET SPEZF21    AT LEVEL 037 AS OF 03/04/05                      
*PHASE T23021A                                                                  
*INCLUDE GETBROAD                                                               
T23021   TITLE 'SPEZF21 - GENERATE WRKR FILE FOR INTERREP'                      
T23021   CSECT                                                                  
***********************************************************************         
*                                                                     *         
*  LEV 03    FEB11/93 CHECK FOR MAX RECORD LENGTH                     *         
*  LEV 04    MAR11/93 FORCE ENTRY OF AGENCY & CK IT AGAINST EST       *         
*  LEV 05    MAR24/93 BYPASS CHECK OF CONVERT ELEM SO THAT OTHER      *         
*                      PRODUCTS GET PICKED UP - ALSO CABLE HEAD       *         
*  LEV 06    JUL02/93 SAVE INVOICE # IN TSPFUSER FROM REQ TO REQ      *         
*                     BYPASS PREVIOUSLY CONVERTED INVOICE ITEMS       *         
*  LEV 07    SEP22/93 FORCE ZERO COST OPTION EZPROF07                 *         
*  LEV 08    FEB10/94 ADD TO TRACE KEYS                               *         
*  LEV 09    FEB15/94 ADD VALIMED TO ALLOW VALISTA                    *         
*  LEV 10    MAR24/94 CK FOR MAX SIZE RECORDS - 360                   *         
*  LEV 11    JUL27/95 ADD CODE FOR NEW INVOICING                      *         
*  LEV 12    AUG01/95 FIX INITSPT, INITXSP                            *         
*  LEV 13    AUG03/95 FIX DISK READ LOOP                              *         
*  LEV 14    AUG15/95 FIX PUTREC FILENAME ERROR                       *         
*  LEV 15    AUG16/95 FIX NEW INVOICE TIME SNVIDTIM KEPT IN MINUTES   *         
*  LEV 16    AUG31/95 BYPASS HEADER UPDATE SNV IF NTH REC IN SET      *         
*                      AND ADD SPOT TOTALS, CORRECT SPOT CTS          *         
*  LEV 17    OCT09/95 ADD BLAIR FOR EZI.SP0Z8BL OUTBL                 *         
*  LEV 18    FEB13/96 FIX BAD BR FOR END OF STATION MONTH             *         
*  LEV 19    FEB27/96 ADD CODE FOR AGENCY XX* - BYPASS EST AGY CK     *         
*  LEV 20    MAR22/96 FIX CODE FOR AGENCY XX* - BYPASS EST AGY CK     *         
*  LEV 21    MAR25/96 ADD CODE FOR APNY TO BYPASS EST CHK             *         
*  LEV 22    APR04/96 ADD CODE FOR LIPA TO BYPASS EST CHK             *         
*  LEV 23 BG MAY21/97 FIX BAD CML CODE - LEFT 9 CHAR, S/B 8           *         
*  LEV 25 BG JUN10/98 ADD PMCNY AS SOURCE                             *         
*  LEV 26 BG MAY27/99 ADD KATZ AS SOURCE                              *         
*  LEV 27 BG MAY23/01 GET RID OF DDS DATES ON OUTPUT                  *         
*  LEV 28 BG MAY24/01 FIX INPUT TO GETBROAD AT STHMHD90               *         
*  LEV 29 BG APR02/02 ADD RMRNY AS SOURCE                             *         
*                                                                     *         
***********************************************************************         
*                                                                     *         
*  TSPFUSER - 0   - 127 = DCB                                         *         
*             128 - 131 = INVOICE COUNTER                             *         
*                                                                     *         
***********************************************************************         
         PRINT NOGEN                                                            
         NMOD1 0,**3021**,R7,RR=R2,CLEAR=YES                                    
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         ST    RC,SVRC                                                          
         ST    R2,RELO                                                          
         MVC   AIO,AIO1                                                         
         MVI   IOOPT,C'Y'          WE'RE DOING ALL THE IO'S                     
         A     R2,=V(GETBROAD)                                                  
         ST    R2,AGTBROAD                                                      
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         CLI   MODE,VALKEY                                                      
         BE    VKEY                                                             
         CLI   MODE,PRINTREP                                                    
         BE    PREC                                                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SEE IF FILE NEEDS TO BE MOVED AT RUNFIRST                                     
***********************************************************************         
RUNF     DS    0H                                                               
*                                                                               
         L     RE,ATWA                                                          
         MVI   29(RE),X'02'        INDICATE RUNLAST HOOK REQUIRED               
*                                                                               
         L     RF,TWADCONS-T230FFD(RE)                                          
         L     R1,TSPFUSER-TWADCOND(RF)                                         
         XC    0(256,R1),0(R1)     ZERO INVOICE CT AREA                         
         SPACE                                                                  
         B     XIT                                                              
         SPACE 3                                                                
***********************************************************************         
* CLOSE FILE AT RUNLAST                                                         
***********************************************************************         
RUNL     DS    0H                                                               
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,ATWA                                                          
         L     RE,TWADCONS-T230FFD(,RE)                                         
         L     RE,TSPFUSER-TWADCOND(,RE) GET DCB ADDRESS                        
*                                                                               
         CLOSE ((RE),)                                                          
         SPACE                                                                  
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY FIELDS                                                       
***********************************************************************         
VKEY     DS    0H                                                               
         MVI   CURSYST,C'M'        SWITCH TO SPOT SYSTEM                        
         GOTO1 VALIFAS                                                          
*                                                                               
         MVI   FILTFLAG,0          CLEAR OUR FLAGS                              
         MVI   BITFLAG1,0                                                       
*****                                                                           
* GET AGENCY INFORMATION                                                        
*****                                                                           
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING AGYKEY,R4                                                        
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,AGENCY                                                   
         DROP  R4                                                               
*                                                                               
         MVC   FILENAME,=CL8'SPTDIR'                                            
         GOTO1 HIGH                                                             
         CLC   KEY(L'INVKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO                                                           
         MVC   FILENAME,=CL8'SPTFIL'                                            
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
*                                                                               
         MVI   ELCODE,X'01'        GET AGENCY DESCRIPTION ELEMENT               
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING AGYEL,R6            SAVE AGENCY NAME                             
         MVC   SGNONAGY,AGYNAME                                                 
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'        GET FIRST MEDIA CODE ELEMENT                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING AGYMEDEL,R6                                                      
         MVC   BAGYMD,AGYMEDBT     SAVE FIRST AGENCY/MEDIA CODE                 
*****                                                                           
* VALIDATE THE MEDIA                                                            
*****                                                                           
VK10     LA    R2,REQMEDH                                                       
         GOTO1 VALIMED             GET MEDIA/SET SPOTNETS FOR VALISTA           
*                                                                               
         CLI   5(R2),0             ANY MEDIA?                                   
         BE    MISSFLD              NO, ERROR                                   
*                                                                               
         CLI   8(R2),C'T'          HAS TO BE ONE OF THESE MEDIAS                
         BE    VK10LP                                                           
         CLI   8(R2),C'R'                                                       
         BE    VK10LP                                                           
         CLI   8(R2),C'N'                                                       
         BE    VK10LP                                                           
         CLI   8(R2),C'X'                                                       
         BNE   INVLFLD                                                          
*                                                                               
VK10LP   CLC   AGYMEDCD,8(R2)      USER MEDIA MATCH THIS MEDIA CODE?            
         BE    VK15                                                             
         BAS   RE,NEXTEL                                                        
         CLI   0(R6),X'02'         STILL MEDIA CODE ELEMENT?                    
         BNE   INVLFLD                                                          
         B     VK10LP              YES                                          
*                                                                               
VK15     MVC   QMED,8(R2)                                                       
         MVC   BAGYMD,AGYMEDBT     SAVE BINARY AGENCY/MEDIA CODE                
         OI    FILTFLAG,FILTMEDQ   FILTER ON MEDIA                              
         DROP  R6                                                               
*****                                                                           
* VALIDATE THE CLIENT                                                           
*****                                                                           
VK20     LA    R2,REQCLTH                                                       
*                                                                               
         CLI   5(R2),0             ANY CLIENT?                                  
         BE    MISSFLD              NO, ERROR                                   
*                                                                               
         CLI   REQMEDH+5,0         IF CLIENT IS SPECIFIED                       
         BE    MISSMED             THEN WE NEED MEDIA                           
*                                                                               
         GOTO1 VALICLT             GET CLTNM                                    
         SPACE                                                                  
* READ EZ PROFILE *                                                             
         SPACE                                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0EZ'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         L     R1,AIO                                                           
         MVC   WORK+11(1),COFFICE-CLTHDRD(R1)                                   
         GOTO1 GETPROF,DMCB,WORK,EZPROF,DATAMGR                                 
         SPACE                                                                  
*                                                                               
         OI    FILTFLAG,FILTCLTQ   FILTER ON CLIENT                             
*****                                                                           
* VALIDATE THE PRODUCT                                                          
*****                                                                           
VK30     LA    R2,REQPRDH                                                       
*                                                                               
         CLI   5(R2),0             ANY PRODUCT?                                 
         BE    MISSFLD              NO                                          
*                                                                               
         CLI   REQMEDH+5,0         IF PRODUCT IS SPECIFIED                      
         BE    MISSMED              THEN WE NEED MEDIA AND CLIENT               
*                                                                               
         CLI   REQCLTH+5,0                                                      
         BE    MISSCLT                                                          
         SPACE                                                                  
         GOTO1 ANY                                                              
         CLC   =C'POL',WORK        USE POL PRODUCT AS ALL PRODUCTS              
         BE    VK40                                                             
*                                                                               
         GOTO1 VALIPRD             GET PRDNM                                    
         MVC   QPRD,WORK                                                        
         MVC   BPRD,WORK+3                                                      
*                                                                               
         OI    FILTFLAG,FILTPRDQ   FILTER ON PRODUCT                            
*****                                                                           
* VALIDATE THE ESTIMATE                                                         
*****                                                                           
VK40     LA    R2,REQESTH                                                       
*                                                                               
         MVI   BEST,0                                                           
         XC    QEST,QEST                                                        
*                                                                               
         CLI   5(R2),0             ANY ESTIMATE?                                
         BNE   VK42                 YES                                         
         CLC   AGENCY,=C'LI'       ALLOW LIPA TO BYPASS ESTIMATE CK             
         BE    VK50                                                             
         CLC   AGENCY,=C'AP'       ALLOW APPLE TO BYPASS ESTIMATE CK            
         BNE   MISSFLD              NO                                          
         B     VK50                                                             
         SPACE                                                                  
VK42     GOTO1 ANY                                                              
         CLI   REQAGY+2,C'*'       THIS A DDS GEN REQUEST                       
         BNE   VK44                                                             
         CLC   =C'000',WORK        USE EST = NO AS ALL ESTIMATES                
         BE    VK50                                                             
         SPACE                                                                  
*        CLC   =C'NO ',WORK        USE EST = NO AS ALL ESTIMATES                
*        BE    VK50                                                             
*                                                                               
VK44     DS    0H                                                               
         CLI   REQMEDH+5,0         IF PRODUCT IS SPECIFIED                      
         BE    MISSMED              THEN WE NEED MEDIA AND CLIENT               
*                                                                               
         CLI   REQCLTH+5,0                                                      
         BE    MISSCLT              THEN WE NEED MEDIA AND CLIENT               
*                                                                               
         CLI   REQPRDH+5,0                                                      
         BE    MISSPRD              THEN WE NEED MEDIA AND CLIENT               
*                                                                               
         GOTO1 VALIEST             GET BEST                                     
*                                                                               
         L     R4,AIO                                                           
         USING ESTHDRD,R4                                                       
         MVC   SVESTAGY,EPROF                                                   
         DROP  R4                                                               
*                                                                               
         OI    FILTFLAG,FILTESTQ   FILTER ON ESTIMATE                           
*****                                                                           
* VALIDATE THE BROADCAST MONTH                                                  
*****                                                                           
VK50     LA    R2,REQMTHH                                                       
*                                                                               
         CLI   5(R2),0             ANY BROADCAST MONTH?                         
         BE    MISSFLD              NO, ERROR                                   
*                                                                               
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK   VALIDATE MONTH/DAY/YEAR             
         OC    DMCB(4),DMCB                                                     
         BNZ   BADDATE                     ONLY ALLOW MONTH/YEAR FORMAT         
*                                                                               
         GOTO1 (RF),(R1),(2,8(R2)),WORK                                         
         OC    DMCB(4),DMCB                                                     
         BZ    BADDATE                                                          
*                                                                               
         MVC   WORK+4(2),=C'01'    FOR NEW INVOICING                            
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(2,FILTYRMO)                                
*                                                                               
         MVC   WORK+4(2),=C'15'    ADD DAY                                      
*                                                                               
         GOTO1 AGTBROAD,(R1),(1,WORK),FILTBROD,GETDAY,ADDAY                     
*                                                                               
         CLI   DMCB,X'FF'          INVALID DATE?                                
         BE    INVLFLD              NO                                          
*                                                                               
         OI    FILTFLAG,FILTBRDQ   FILTER ON BROADCAST MONTH                    
         EJECT                                                                  
*****                                                                           
* VALIDATE THE AGENCY POWER CODE                                                
*****                                                                           
VK60     LA    R2,REQAGYH                                                       
*                                                                               
         CLI   5(R2),0             ANY AGENCY POWER CODE?                       
         BE    MISSFLD              NO                                          
*                                                                               
         CLI   OFFLINE,C'Y'        IS THIS OFFLINE                              
         BE    VK64                                                             
         CLI   TWAOFFC,C'*'        THIS A DDS TERMINAL                          
         BE    VK64                                                             
*                                                                               
         CLI   5(R2),2             MUST BE LENGTH OF 2                          
         BNE   INVLFLD                                                          
*                                                                               
VK64     MVC   HALF,8(R2)                                                       
         GOTO1 =A(GETAGYNM),RR=RELO                                             
*                                                                               
         CLI   REQAGY+2,C'*'       THIS A DDS GENERATED REQUEST                 
         BE    VK66                 YES, BYPASS ESTIMATE CHECK                  
*                                                                               
         CLI   BEST,0              IF EST IS ZERO                               
         BNE   VK65                 NO, DO ESTIMATE CHECK                       
*                                                                               
         CLC   AGENCY,=C'AP'       ALLOW APPLE TO BYPASS ESTIMATE CK            
         BE    VK66                 YES, BYPASS ESTIMATE CHECK                  
*                                                                               
         CLC   AGENCY,=C'LI'       ALLOW LIPA TO BYPASS ESTIMATE CK             
         BE    VK66                 YES, BYPASS ESTIMATE CHECK                  
*                                                                               
VK65     CLC   REQAGY(2),SVESTAGY  THIS THE SAME AS ESTIMATE                    
         BNE   BADAGYCD                                                         
*                                                                               
VK66     OI    FILTFLAG,FILTAGYQ   FILTER ON AGENCY                             
*****                                                                           
* VALIDATE THE STATION                                                          
*****                                                                           
VK70     LA    R2,REQSTAH                                                       
*                                                                               
         CLI   5(R2),0             ANY STATION?                                 
         BE    VK80                NO                                           
*                                                                               
         CLI   REQMEDH+5,0         IF STATION IS SPECIFIED                      
         BE    MISSMED             THEN WE NEED MEDIA                           
*                                                                               
         CLI   REQCLTH+5,0                                                      
         BE    MISSCLT                  AND WE NEED CLIENT                      
*                                                                               
         GOTO1 VALISTA             GET QSTA                                     
*                                                                               
         MVI   BLOCK,C'0'          NO MARKET                                    
         MVC   BLOCK+1(3),BLOCK                                                 
*                                                                               
         GOTO1 MSPACK,DMCB,BLOCK,QSTA,BMKTSTA                                   
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    FILTFLAG,FILTSTAQ   FILTER ON STATION                            
*****                                                                           
* VALIDATE THE RERUN DATE                                                       
*****                                                                           
VK80     LA    R2,REQRRUNH                                                      
         XC    RRUNDATE,RRUNDATE                                                
*                                                                               
         CLI   5(R2),0                                                          
         BE    VK90                                                             
*                                                                               
         CLI   OFFLINE,C'Y'        ONLY ACCEPT IF OFFLINE                       
         BNE   INVLFLD              (STOPS AGENCIES FROM USING ONLINE)          
*                                                                               
         LA    R3,8(R2)                                                         
         ICM   R3,8,5(R2)                                                       
         GOTO1 PERVAL,DMCB,(R3),(X'60',PERVALST)                                
         CLI   DMCB+4,4                                                         
         BE    *+12                                                             
         CLI   DMCB+4,0                                                         
         BNE   INVLFLD                                                          
*                                                                               
         LA    R1,PERVALST                                                      
         USING PERVALD,R1                                                       
         MVC   RRUNDATE,PVALBSTA                                                
         DROP  R1                                                               
*                                                                               
         OI    FILTFLAG,FILTRRNQ   FILTER ON RERUN DATE                         
*****                                                                           
* DONE VALIDATE FIELDS ON THE SCREEN                                            
*****                                                                           
VK90     DS    0H                                                               
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS THE USER REQUEST                                                      
***********************************************************************         
PREC     DS    0H                                                               
         LOAD  EP=T23022,ERRET=LOADERR                                          
         B     *+6                                                              
LOADERR  DC    H'0'                                                             
*                                                                               
         ST    R0,AESTMTBL         ADDRESS OF OUR ESTIMATE TABLE                
         AHI   R0,LESTMTBL                                                      
         ST    R0,ASFLMTBL         ADDRESS OF OUR SAVED FILM TABLE              
         AHI   R0,LSFLMTBL                                                      
         ST    R0,AFILMTBL         ADDRESS OF OUR FILM TABLE                    
         AHI   R0,LFILMTBL                                                      
         ST    R0,AINVNTBL         ADDRESS OF OUR INVOICE NUMBER TABLE          
         AHI   R0,LINVNTBL                                                      
         ST    R0,AITMSTBL         ADDRESS OF OUR INVOICE ITEM TABLE            
*                                                                               
         MVI   ACTELOPT,C'N'                                                    
*                                                                               
         L     R1,=A(HEADING)                                                   
         ST    R1,SPECS                                                         
*                                                                               
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000A50'  GET A(QSORT)                             
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                DIE IF WE CAN'T GET ADDRESS                  
         MVC   QSORT,DMCB                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(X'20',TODAYDTE)                               
*                                                                               
         MVI   SNTAGYMD,0          MEDIA WAS NOT SENT YET                       
         XC    SNTSTATN,SNTSTATN   STATION WAS NOT SENT YET                     
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,ATWA                                                          
         L     RF,TWADCONS-T230FFD(,R1)                                         
         L     RE,TSPFUSER-TWADCOND(,RF) GET DCB ADDRESS                        
         ST    RE,AOUTFILE                                                      
*                                                                               
         MVC   OURINVN,128(RE)           START WITH LAST REQ INVOICE #          
*                                                                               
         OC    0(128,RE),0(RE)           TEST FIRST TIME (NMOD PRESENT)         
         BNZ   PROPENED                                                         
         SPACE                                                                  
         L     RF,=A(OUTFILE)                                                   
         MVC   0(128,RE),0(RF)     MOVE DCB                                     
         SPACE                                                                  
*&&DO                                                                           
         CLC   AGENCY,IRPC         INTER-REP                                    
         BNE   PREC04                                                           
         MVC   43(5,RE),IRPC                                                    
         B     PREC20                                                           
PREC04   CLC   AGENCY,APPC         APPLE                                        
         BNE   PREC05                                                           
         MVC   43(5,RE),APPC                                                    
         B     PREC20                                                           
PREC05   CLC   AGENCY,K3PC         KATZ                                         
         BNE   PREC06                                                           
         MVC   43(5,RE),K3PC                                                    
         B     PREC20                                                           
PREC06   CLC   AGENCY,LIPC         LIPA                                         
         BNE   PREC08                                                           
         MVC   43(5,RE),LIPC                                                    
         B     PREC20                                                           
PREC08   CLC   AGENCY,PTPC         PMCNY                                        
         BNE   PREC10                                                           
         MVC   43(5,RE),PTPC                                                    
         B     PREC20                                                           
PREC10   CLC   AGENCY,BLPC         BLAIR                                        
         BNE   PREC12                                                           
         MVC   43(5,RE),BLPC                                                    
         B     PREC20                                                           
*                                                                               
PREC12   CLC   AGENCY,RAPC         RMRNY REGIONAL MARKET RADIO                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   43(5,RE),RAPC                                                    
*&&                                                                             
         MVC   43(5,RE),SPACES                                                  
         MVC   43(2,RE),AGENCY                                                  
*                                                                               
PREC20   DS    0H                                                               
         OPEN  ((RE),OUTPUT)                                                    
*                                                                               
         LTR   RF,RF               WAS OPEN DONE OK                             
         BZ    *+6                                                              
         DC    H'0'    FAILED TO OPEN OUTPUT FILE FOR THIS AGY                  
*                                                                               
PROPENED XC    LSTINVKY,LSTINVKY   CLEAR THE LAST INVOICE KEY                   
         MVC   LSTAGYMD,BAGYMD     SAVE FIRST AGENCY/MEDIA FOR LATER            
         XC    LSTSTATN,LSTSTATN   THE REST HAVE NO INFORMATION                 
         XC    LSTCLNT,LSTCLNT                                                  
         MVI   NUMBESTM,0          NO ESTIMATES IN OUR TABLE YET                
         XC    LSTBDMTH,LSTBDMTH                                                
         XC    TOTNUMIN,TOTNUMIN   TOTAL NUMBER OF INVOICES                     
         XC    TOTNUMSP,TOTNUMSP   TOTAL NUMBER OF SPOTS                        
         ZAP   GRSTOTAL,=P'0'      GROSS TOTAL FROM INVOICES                    
*                                                                               
PRSETKEY XC    KEY,KEY             SET UP THE INVOICE KEY                       
         LA    R4,KEY                                                           
         USING INVKEY,R4                                                        
         MVI   INVKCOD,X'0B'                                                    
*                                                                               
         MVC   INVKAM,LSTAGYMD                                                  
         TM    FILTFLAG,FILTMEDQ   MEDIA INPUTTED?                              
         BZ    *+10                                                             
         MVC   INVKAM,BAGYMD       YES                                          
*                                                                               
         MVC   INVKSTA,LSTSTATN                                                 
         TM    FILTFLAG,FILTSTAQ   STATION INPUTTED?                            
         BZ    *+10                                                             
         MVC   INVKSTA,BSTA          THEN USE THE STATION CODE                  
*                                                                               
         MVC   INVKCLT,LSTCLNT                                                  
         TM    FILTFLAG,FILTCLTQ   CLIENT INPUTTED?                             
         BZ    *+10                                                             
         MVC   INVKCLT,BCLT        YES                                          
*                                                                               
         TM    FILTFLAG,FILTBRDQ   BROADCAST MONTH INPUTTED?                    
         BZ    *+14                                                             
         MVC   DUB(L'FILTBRST),FILTBRST   YES                                   
         B     PRSTKY10                                                         
*                                                                               
         OC    LSTBDMTH,LSTBDMTH                                                
         BZ    PRHIGH                                                           
         MVC   DUB(L'LSTBDDTS),LSTBDDTS                                         
*                                                                               
PRSTKY10 GOTO1 DATCON,DMCB,(0,DUB),(2,INVKDAT)                                  
*****                                                                           
* GET THE FIRST KEY                                                             
*****                                                                           
PRHIGH   DS    0H                                                               
         L     R1,AFILMTBL         RESET THE TABLES FIRST                       
         MVI   0(R1),X'FF'                                                      
         MVI   NUMBFILM,0          RESET NUMBER OF FILM CODES                   
*                                                                               
         L     R1,AITMSTBL                                                      
         ST    R1,ANXTITMS                                                      
         XC    NUMBITMS,NUMBITMS   RESET NUMBER OF INVOICE ITEMS                
*                                                                               
         L     R1,AINVNTBL                                                      
         ST    R1,ANXTINVN                                                      
         XC    NUMBINVN,NUMBINVN   RESET NUMBER OF INVOICE NUMBERS              
*                                                                               
         MVC   FILENAME,=CL8'SPTDIR'                                            
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
*                                                                               
PRTSTKEY CLI   INVKCOD,X'0B'       STILL AN INVOICE RECORD?                     
         BNE   PROPENNW                                                         
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    *+12                                                             
         CLI   TWAOFFC,C'*'        THIS A DDS TERMINAL                          
         BNE   PR010                                                            
         CLI   REQTRCE,C'Y'                                                     
         BNE   PR010                                                            
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
          MVC   P(13),=CL13'INVOICE KEY: '                                      
          GOTO1 HEXOUT,DMCB,INVKEY,P+14,L'INVKEY                                
          MVC  P+50(5),QSTA                                                     
          MVC  P+60(5),=C'BSTA='                                                
          GOTO1 HEXOUT,DMCB,BSTA,P+65,3                                         
          MVC  P+80(9),=C'LSTSTATN='                                            
          GOTO1 HEXOUT,DMCB,LSTSTATN,P+90,3                                     
          GOTO1 SPOOL,DMCB,(R8)                                                 
*****                                                                           
* CHECK THE AGENCY/MEDIA IN THE KEY                                             
*****                                                                           
PR010    TM    FILTFLAG,FILTMEDQ   MEDIA SPECIFIED?                             
         BNZ   PR015                YES                                         
*                                                                               
         CLC   INVKAM,LSTAGYMD     AGENCY/MEDIA SAME AS LAST ONE?               
         BE    PR020                YES                                         
*                                                                               
         BAS   RE,ANYITMS                                                       
*                                                                               
         MVC   HALF(1),INVKAM      NO, CHECK IF SAME AGENCY                     
         NI    HALF,X'F0'                                                       
         MVC   HALF+1(1),LSTAGYMD                                               
         NI    HALF+1,X'F0'                                                     
*                                                                               
         CLC   HALF(1),HALF+1      SAME AGENCY, DIFFERENT MEDIA?                
         BNE   PROPENNW                                                         
*                                                                               
         XC    LSTSTATN,LSTSTATN                                                
         XC    LSTCLNT,LSTCLNT                                                  
         MVI   NUMBESTM,0          CLEAR ESTIMATE TABLE IF NEW AGY/MED          
         XC    LSTBDMTH,LSTBDMTH                                                
         MVC   LSTAGYMD,INVKAM     LATEST AGENCY/MEDIA CODE                     
*                                                                               
PR012    MVC   BYTE,INVKAM                                                      
         NI    BYTE,X'0F'                                                       
         ZIC   R1,BYTE                                                          
         LA    R1,MEDIALST-1(R1)                                                
         MVC   QMED,0(R1)          GET CORRESPONDING EBCDIC MEDIA               
         MVC   BAGYMD,INVKAM                                                    
         B     PRSETKEY                                                         
*                                                                               
PR015    CLC   INVKAM,BAGYMD       IF THE (AGENCY/MEDIA)S ARE NOT EQUAL         
         BE    PR020                                                            
         BAS   RE,ANYITMS          THEN SEE IF ANY ITEMS TO SHOW                
         B     PROPENNW                AND EXIT                                 
*****                                                                           
* CHECK THE STATION IN THE KEY                                                  
*****                                                                           
PR020    TM    FILTFLAG,FILTSTAQ   STATION SPECIFIED?                           
         BNZ   PR025                                                            
*                                                                               
         CLC   INVKSTA,LSTSTATN    STATION SAME AS WE LAST SAW?                 
         BE    PR030                YES                                         
*                                                                               
         BAS   RE,ANYITMS                                                       
*                                                                               
         XC    LSTCLNT,LSTCLNT                                                  
         MVI   NUMBESTM,0          CLEAR ESTIMATE TABLE IF NEW STATION          
         XC    LSTBDMTH,LSTBDMTH                                                
         MVC   LSTSTATN,INVKSTA    NO, COPY THE STATION WE HAVE NOW             
*                                                                               
PR022    XC    LSTMRKT,LSTMRKT                                                  
         MVC   BSTA,INVKSTA                                                     
         GOTO1 MSUNPK,DMCB,(X'80',LSTMRKT),QMKT,WORK                            
         MVC   QSTA,WORK                                                        
         SPACE                                                                  
         XC    QNET,QNET                                                        
         CLC   WORK+5(3),SPACES                                                 
         BE    PRSETKEY                                                         
         MVC   QNET,WORK+5                                                      
         B     PRSETKEY                                                         
*                                                                               
PR025    CLC   INVKSTA,BSTA        IF THE STATIONS ARE NOT EQUAL                
         BE    PR030                                                            
         BL    PRSETKEY            THEN SET THE KEY UP IF LOWER                 
*                                                                               
         BAS   RE,ANYITMS                                                       
*                                                                               
         TM    FILTFLAG,FILTMEDQ   MEDIA SPECIFIED?                             
         BNZ   PROPENNW            YES, THEN WE'RE DONE                         
PRINCMED ZIC   R1,LSTAGYMD         KEY'S STATION IS HIGHER                      
         LA    R1,1(R1)                                                         
         STC   R1,LSTAGYMD         USE NEXT AGENCY/MEDIA CODE                   
         MVC   INVKAM,LSTAGYMD                                                  
         XC    LSTCLNT,LSTCLNT                                                  
         MVI   NUMBESTM,0          CLEAR ESTIMATE TABLE IF NEW STATION          
         XC    LSTBDMTH,LSTBDMTH                                                
         B     PR012               SET QMED                                     
         EJECT                                                                  
*****                                                                           
* CHECK THE CLIENT IN THE KEY                                                   
*****                                                                           
PR030    TM    FILTFLAG,FILTCLTQ   CLIENT SPECIFIED?                            
         BNZ   PR035                                                            
*                                                                               
         CLC   INVKCLT,LSTCLNT     CLIENT SAME AS WE LAST SAW?                  
         BE    PR040               YES                                          
*                                                                               
         BAS   RE,ANYITMS                                                       
*                                                                               
         MVI   NUMBESTM,0          CLEAR ESTIMATE TABLE IF NEW CLIENT           
         XC    LSTBDMTH,LSTBDMTH                                                
         MVC   LSTCLNT,INVKCLT     NO, COPY THE CLIENT WE HAVE NOW              
*                                                                               
PR032    MVC   BCLT,INVKCLT                                                     
         GOTO1 CLUNPK,DMCB,BCLT,QCLT                                            
         B     PRSETKEY                                                         
*                                                                               
PR035    CLC   INVKCLT,BCLT        IF THE CLIENTS ARE NOT EQUAL                 
         BE    PR040                                                            
         BL    PRSETKEY            THEN SET THE KEY UP IF LOWER                 
*                                                                               
         BAS   RE,ANYITMS                                                       
*                                                                               
         TM    FILTFLAG,FILTMEDQ+FILTSTAQ  MED & STA BOTH SPECIFIED?            
         BO    PROPENNW            YES, THEN WE'RE DONE                         
*                                                                               
PRINCSTA TM    FILTFLAG,FILTSTAQ   STATION SPECIFIED?                           
         BNZ   PRINCMED                                                         
         ZICM  R1,LSTSTATN,3       NO, INCREMENT LAST STATION USED              
         LA    R1,1(R1)                                                         
         STCM  R1,7,LSTSTATN                                                    
         MVC   INVKSTA,LSTSTATN                                                 
         MVI   NUMBESTM,0          CLEAR ESTIMATE TABLE IF NEW CLIENT           
         XC    LSTBDMTH,LSTBDMTH                                                
         B     PR022               GET QSTA                                     
*****                                                                           
* CHECK THE DATE IN THE KEY                                                     
*****                                                                           
PR040    GOTO1 DATCON,DMCB,(2,INVKDAT),(0,BLOCK)                                
*                                                                               
         GOTO1 AGTBROAD,(R1),(1,BLOCK),BRDAREA,GETDAY,ADDAY                     
*                                                                               
         TM    FILTFLAG,FILTBRDQ   BROADCAST MONTH SPECIFIED?                   
         BNZ   PR045                                                            
*                                                                               
         CLC   LSTBDMTH,BRDAREA    SAME BROADCAST MONTH?                        
         BE    PR050               YES                                          
*                                                                               
         BAS   RE,ANYITMS                                                       
*                                                                               
         MVC   LSTBDMTH,BRDAREA                                                 
         B     PRSETKEY                                                         
*                                                                               
PR045    CLC   BRDAREA,FILTBROD    SAME BROADCAST MONTH?                        
         BE    PR050                                                            
         BL    PRSETKEY                                                         
*                                                                               
         BAS   RE,ANYITMS                                                       
*                                  MED, STA, & CLT  ALL  SPECIFIED?             
         TM    FILTFLAG,FILTMEDQ+FILTSTAQ+FILTCLTQ                              
         BO    PROPENNW            YES, THEN WE'RE DONE                         
*                                                                               
PRINCCLT TM    FILTFLAG,FILTCLTQ   CLIENT SPECIFIED?                            
         BNZ   PRINCSTA                                                         
         ZICM  R1,LSTCLNT,2        NO, INCREMENT LAST CLIENT USED               
         LA    R1,1(R1)                                                         
         STCM  R1,3,LSTCLNT                                                     
         MVC   INVKCLT,LSTCLNT                                                  
         B     PR032               GET QCLT                                     
*                                                                               
         DROP  R4                                                               
*****                                                                           
* DONE CHECKING PARTS OF THE KEY                                                
*****                                                                           
PR050    MVC   FILENAME,=CL8'SPTFIL'  GET THE INVOICE RECORD                    
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
*                                                                               
         L     R6,AIO                                                           
         USING INVKEY,R6                                                        
*                                                                               
         GOTO1 DATCON,DMCB,(2,INVKDAT),(0,ADATE)                                
         GOTO1 AGTBROAD,(R1),(1,ADATE),KEYBROAD,GETDAY,ADDAY                    
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    *+12                                                             
         CLI   TWAOFFC,C'*'        THIS A DDS TERMINAL                          
         BNE   PR060                                                            
         CLI   REQTRCE,C'Y'                                                     
         BNE   PR060                                                            
*                                                                               
         MVC   P(16),=CL16'INVOICE RECORD: '                                    
         GOTO1 HEXOUT,DMCB,INVKEY,P+17,L'INVKEY                                 
         DROP  R6                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PR060    BAS   RE,PUTINBUF         PUT INVOICE ITEMS INTO OUR BUFFER            
*                                                                               
PRNEXT   MVC   FILENAME,=CL8'SPTDIR'  GET NEXT INVOICE RECORD                   
         GOTO1 SEQ                                                              
         XC    FILENAME,FILENAME                                                
         B     PRTSTKEY            TEST THE KEY                                 
         EJECT                                                                  
* NOW READ ANY NEW INVOICES                                                     
         SPACE                                                                  
PROPENNW XC    LSTINVKY,LSTINVKY   CLEAR THE LAST INVOICE KEY                   
         MVC   LSTAGYMD,BAGYMD     SAVE FIRST AGENCY/MEDIA FOR LATER            
         XC    LSTCLNT,LSTCLNT                                                  
         XC    LSTSTATN,LSTSTATN                                                
         XC    LSTINVNO,LSTINVNO                                                
         XC    LSTBDMTH,LSTBDMTH                                                
         MVI   NUMBESTM,0          NO ESTIMATES IN OUR TABLE YET                
*                                                                               
PRSETNWK BAS   RE,INITXSP                                                       
         XC    KEY,KEY             SET UP THE INVOICE KEY                       
         LA    R4,KEY                                                           
         USING SNVKEY,R4                                                        
         MVI   SNVKTYPE,SNVKTYPQ                                                
         MVI   SNVKSUB,SNVKSUBQ                                                 
*                                                                               
         MVC   SNVKAM,LSTAGYMD                                                  
         TM    FILTFLAG,FILTMEDQ   MEDIA INPUTTED?                              
         BZ    *+10                                                             
         MVC   SNVKAM,BAGYMD       YES                                          
*                                                                               
         MVC   SNVKCLT,LSTCLNT                                                  
         TM    FILTFLAG,FILTCLTQ   CLIENT INPUTTED?                             
         BZ    *+10                                                             
         MVC   SNVKCLT,BCLT        YES                                          
*                                                                               
         MVC   SNVKSTA,LSTSTATN                                                 
         TM    FILTFLAG,FILTSTAQ   STATION INPUTTED?                            
         BZ    *+10                                                             
         MVC   SNVKSTA,BSTA          THEN USE THE STATION CODE                  
*                                                                               
         TM    FILTFLAG,FILTBRDQ   BROADCAST MONTH INPUTTED?                    
         BZ    *+20                                                             
         MVC   HALF,FILTYRMO        YES                                         
         XC    HALF,=X'FFFF'                                                    
         B     PRSTNK10                                                         
*                                                                               
         OC    LSTBDMTH,LSTBDMTH                                                
         BZ    PRHIGHNW                                                         
         MVC   HALF,LSTBDMTH                                                    
*                                                                               
PRSTNK10 MVC   SNVKMOS,HALF                                                     
*****                                                                           
* GET THE FIRST KEY                                                             
*****                                                                           
PRHIGHNW DS    0H                                                               
         L     R1,AFILMTBL         RESET THE TABLES FIRST                       
         MVI   0(R1),X'FF'                                                      
         MVI   NUMBFILM,0          RESET NUMBER OF FILM CODES                   
*                                                                               
         L     R1,AITMSTBL                                                      
         ST    R1,ANXTITMS                                                      
         XC    NUMBITMS,NUMBITMS   RESET NUMBER OF INVOICE ITEMS                
*                                                                               
         L     R1,AINVNTBL                                                      
         ST    R1,ANXTINVN                                                      
         XC    NUMBINVN,NUMBINVN   RESET NUMBER OF INVOICE NUMBERS              
*                                                                               
         MVC   FILENAME,=CL8'XSPDIR'                                            
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
*                                                                               
PRTSTNWK CLI   SNVKTYPE,SNVKTYPQ    STILL AN INVOICE RECORD?                    
         BNE   PRTRANS                                                          
         CLI   SNVKSUB,SNVKSUBQ     STILL AN INVOICE RECORD?                    
         BNE   PRTRANS                                                          
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    *+12                                                             
         CLI   TWAOFFC,C'*'        THIS A DDS TERMINAL                          
         BNE   PR110                                                            
         CLI   REQTRCE,C'Y'                                                     
         BNE   PR110                                                            
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   P(13),=CL13'INVOICE KEY: '                                       
         GOTO1 HEXOUT,DMCB,SNVKEY,P+14,L'SNVKEY                                 
         MVC  P+80(5),QSTA                                                      
         MVC  P+87(5),=C'BSTA='                                                 
         GOTO1 HEXOUT,DMCB,BSTA,P+92,3                                          
         MVC  P+100(9),=C'LSTSTATN='                                            
         GOTO1 HEXOUT,DMCB,LSTSTATN,P+110,3                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
*****                                                                           
* CHECK THE AGENCY/MEDIA IN THE KEY                                             
*****                                                                           
PR110    TM    FILTFLAG,FILTMEDQ   MEDIA SPECIFIED?                             
         BNZ   PR115                YES                                         
*                                                                               
         CLC   SNVKAM,LSTAGYMD     AGENCY/MEDIA SAME AS LAST ONE?               
         BE    PR120                YES                                         
*                                                                               
         BAS   RE,ANYITMS                                                       
*                                                                               
         MVC   HALF(1),SNVKAM      NO, CHECK IF SAME AGENCY                     
         NI    HALF,X'F0'                                                       
         MVC   HALF+1(1),LSTAGYMD                                               
         NI    HALF+1,X'F0'                                                     
*                                                                               
         CLC   HALF(1),HALF+1      SAME AGENCY, DIFFERENT MEDIA?                
         BNE   PRTRANS                                                          
*                                                                               
         XC    LSTCLNT,LSTCLNT                                                  
         XC    LSTSTATN,LSTSTATN                                                
         XC    LSTINVNO,LSTINVNO                                                
         MVI   NUMBESTM,0          CLEAR ESTIMATE TABLE IF NEW AGY/MED          
         XC    LSTBDMTH,LSTBDMTH                                                
         MVC   LSTAGYMD,SNVKAM     LATEST AGENCY/MEDIA CODE                     
*                                                                               
PR112    MVC   BYTE,SNVKAM                                                      
         NI    BYTE,X'0F'                                                       
         ZIC   R1,BYTE                                                          
         LA    R1,MEDIALST-1(R1)                                                
         MVC   QMED,0(R1)          GET CORRESPONDING EBCDIC MEDIA               
         MVC   BAGYMD,SNVKAM                                                    
         B     PRSETNWK                                                         
*                                                                               
PR115    CLC   SNVKAM,BAGYMD       IF THE (AGENCY/MEDIA)S ARE NOT EQUAL         
         BE    PR120                                                            
         BAS   RE,ANYITMS          THEN SEE IF ANY ITEMS TO SHOW                
         B     PRTRANS                 AND EXIT                                 
*****                                                                           
* CHECK THE CLIENT IN THE KEY                                                   
*****                                                                           
PR120    TM    FILTFLAG,FILTCLTQ   CLIENT SPECIFIED?                            
         BNZ   PR125                                                            
*                                                                               
         CLC   SNVKCLT,LSTCLNT     CLIENT SAME AS WE LAST SAW?                  
         BE    PR130                YES                                         
*                                                                               
         BAS   RE,ANYITMS                                                       
*                                                                               
         MVC   LSTCLNT,SNVKCLT     COPY THE CLIENT WE HAVE NOW                  
         MVI   NUMBESTM,0          CLEAR ESTIMATE TABLE IF NEW CLIENT           
         XC    LSTSTATN,LSTSTATN                                                
         XC    LSTINVNO,LSTINVNO                                                
         XC    LSTBDMTH,LSTBDMTH                                                
*                                                                               
         MVC   BCLT,SNVKCLT                                                     
         GOTO1 CLUNPK,DMCB,BCLT,QCLT                                            
         B     PRSETNWK                                                         
*                                                                               
PR125    CLC   SNVKCLT,BCLT        IF THE CLIENTS ARE NOT EQUAL                 
         BE    PR130                                                            
         BL    PRSETNWK            THEN SET THE KEY UP IF LOWER                 
*                                                                               
         BAS   RE,ANYITMS                                                       
*                                                                               
         TM    FILTFLAG,FILTMEDQ   MEDIA SPECIFIED?                             
         BNZ   PRTRANS             YES, THEN WE'RE DONE                         
*                                                                               
         ZIC   R1,LSTAGYMD         KEY'S CLIENT IS HIGHER                       
         LA    R1,1(R1)                                                         
         STC   R1,LSTAGYMD         USE NEXT AGENCY/MEDIA CODE                   
         MVC   SNVKAM,LSTAGYMD                                                  
         MVC   LSTCLNT,SNVKCLT                                                  
         XC    LSTSTATN,LSTSTATN                                                
         XC    LSTBDMTH,LSTBDMTH                                                
         XC    LSTINVNO,LSTINVNO                                                
         MVI   NUMBESTM,0          CLEAR ESTIMATE TABLE IF NEW STATION          
         B     PR112               SET QMED                                     
         EJECT                                                                  
*****                                                                           
* CHECK THE STATION IN THE KEY                                                  
*****                                                                           
PR130    TM    FILTFLAG,FILTSTAQ   STATION SPECIFIED?                           
         BNZ   PR135                                                            
*                                                                               
         CLC   SNVKSTA,LSTSTATN    STATION SAME AS WE LAST SAW?                 
         BE    PR140                YES                                         
*                                                                               
         BAS   RE,ANYITMS                                                       
*                                                                               
         MVC   LSTSTATN,SNVKSTA    NO, COPY THE STATION WE HAVE NOW             
         XC    LSTBDMTH,LSTBDMTH                                                
         XC    LSTINVNO,LSTINVNO                                                
         MVI   NUMBESTM,0          CLEAR ESTIMATE TABLE IF NEW STATION          
*                                                                               
PR132    XC    LSTMRKT,LSTMRKT                                                  
         MVC   BSTA,SNVKSTA                                                     
         GOTO1 MSUNPK,DMCB,(X'80',LSTMRKT),QMKT,WORK                            
         MVC   QSTA,WORK                                                        
         SPACE                                                                  
         XC    QNET,QNET                                                        
         CLC   WORK+5(3),SPACES                                                 
         BE    PRSETNWK                                                         
         MVC   QNET,WORK+5                                                      
         B     PRSETNWK                                                         
*                                                                               
PR135    CLC   SNVKSTA,BSTA        IF THE STATIONS ARE NOT EQUAL                
         BE    PR140                                                            
         BL    PRSETNWK            THEN SET THE KEY UP IF LOWER                 
*                                                                               
         BAS   RE,ANYITMS                                                       
*                                                                               
         TM    FILTFLAG,FILTMEDQ+FILTCLTQ   MEDIA & CLIENT SPECIFIED?           
         BNZ   PRTRANS                       YES, THEN WE'RE DONE               
*                                                                               
PRINCMNW ZICM  R1,LSTCLNT,2       KEY'S STATION IS HIGHER                       
         LA    R1,1(R1)                                                         
         STCM  R1,3,LSTCLNT        USE NEXT CLIENT                              
         XC    LSTSTATN,LSTSTATN                                                
         XC    LSTBDMTH,LSTBDMTH                                                
         XC    LSTINVNO,LSTINVNO                                                
         MVI   NUMBESTM,0          CLEAR ESTIMATE TABLE IF NEW STATION          
         B     PR112               SET QMED                                     
*                                                                               
PRINCSNW TM    FILTFLAG,FILTSTAQ   STATION SPECIFIED?                           
         BNZ   PRINCMNW                                                         
         ZICM  R1,LSTSTATN,3       NO, INCREMENT LAST STATION USED              
         LA    R1,1(R1)                                                         
         STCM  R1,7,LSTSTATN                                                    
         MVC   SNVKSTA,LSTSTATN                                                 
         MVI   NUMBESTM,0          CLEAR ESTIMATE TABLE IF NEW CLIENT           
         XC    LSTBDMTH,LSTBDMTH                                                
         XC    LSTINVNO,LSTINVNO                                                
         B     PR132               GET QSTA                                     
*****                                                                           
* CHECK THE DATE IN THE KEY                                                     
*****                                                                           
PR140    MVC   HALF,SNVKMOS                                                     
         XC    HALF,=X'FFFF'                                                    
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(2,HALF),(0,DUB)                                     
*                                                                               
         MVC   DUB+4(2),=C'15'                                                  
*                                                                               
         GOTO1 AGTBROAD,(R1),(1,DUB),BRDAREA,GETDAY,ADDAY                       
*                                                                               
         TM    FILTFLAG,FILTBRDQ   BROADCAST MONTH SPECIFIED?                   
         BNZ   PR145                                                            
*                                                                               
         CLC   LSTBDMTH,SNVKMOS    SAME BROADCAST MONTH?                        
         BE    PR150                YES                                         
*                                                                               
         BAS   RE,ANYITMS                                                       
*                                                                               
         MVC   LSTBDMTH,SNVKMOS                                                 
         XC    LSTINVNO,LSTINVNO                                                
         B     PRSETNWK                                                         
*                                                                               
PR145    CLC   HALF,FILTYRMO       SAME BROADCAST MONTH?                        
         BE    PR150                                                            
*                                                                               
         BAS   RE,ANYITMS                                                       
*                                  MED, STA, & CLT  ALL  SPECIFIED?             
         TM    FILTFLAG,FILTMEDQ+FILTSTAQ+FILTCLTQ                              
         BO    PRTRANS             YES, THEN WE'RE DONE                         
*                                                                               
PRINCCNW TM    FILTFLAG,FILTSTAQ   STATION SPECIFIED?                           
         BNZ   PRINCSNW                                                         
         ZICM  R1,LSTSTATN,3       NO, INCREMENT LAST STATION USED              
         LA    R1,1(R1)                                                         
         STCM  R1,7,LSTSTATN                                                    
         MVC   SNVKSTA,LSTSTATN                                                 
         B     PR132               GET NEXT STATION                             
*                                                                               
         DROP  R4                                                               
*****                                                                           
* DONE CHECKING PARTS OF THE KEY                                                
*****                                                                           
PR150    MVC   FILENAME,=CL8'XSPFIL'  GET THE INVOICE RECORD                    
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
*                                                                               
         L     R6,AIO                                                           
         USING SNVKEY,R6                                                        
*                                                                               
         MVC   HALF,SNVKMOS                                                     
         XC    HALF,=X'FFFF'                                                    
         GOTO1 DATCON,DMCB,(2,HALF),(0,ADATE)                                   
         MVC   ADATE+4(2),=C'15'                                                
*                                                                               
         GOTO1 AGTBROAD,(R1),(1,ADATE),KEYBROAD,GETDAY,ADDAY                    
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    *+12                                                             
         CLI   TWAOFFC,C'*'        THIS A DDS TERMINAL                          
         BNE   PR160                                                            
         CLI   REQTRCE,C'Y'                                                     
         BNE   PR160                                                            
*                                                                               
         MVC   P(13),=CL16'INVOICE REC: '                                       
         GOTO1 HEXOUT,DMCB,SNVKEY,P+14,L'SNVKEY                                 
         DROP  R6                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PR160    BAS   RE,PUTINBNW         PUT INVOICE ITEMS INTO OUR BUFFER            
*                                                                               
         MVC   FILENAME,=CL8'XSPDIR'  GET NEXT INVOICE RECORD                   
         GOTO1 SEQ                                                              
         XC    FILENAME,FILENAME                                                
         SPACE                                                                  
         NI    BITFLAG2,X'FF'-BTITMULT    SET OFF MULTI RECORD SET              
         SPACE                                                                  
         L     R6,AIO                                                           
         CLC   KEY(L'SNVKMAST),0(R6)                                            
         BNE   PRTSTNWK            TEST THE KEY                                 
         SPACE                                                                  
         OI    BITFLAG2,BTITMULT   SET ON MULTI RECORD SET                      
         B     PR150                                                            
         EJECT                                                                  
* PUT OUT TOTAL RECORD AND PRINT TOTALS - END OF REQUEST *                      
*                                                                               
PRTRANS  DS    0H                                                               
         XC    OUTRECLN,OUTRECLN                                                
         OC    TOTNUMIN,TOTNUMIN   ANY INVOICES WRITTEN                         
         BZ    PRTR40               NO                                          
         SPACE                                                                  
         LA    R3,OUTDATA                                                       
         MVC   0(2,R3),=C'12'      TRANSMISSION TOTAL CODE                      
         MVI   2(R3),X'5E'                                                      
         LA    R3,3(R3)                                                         
         EDIT  (B4,TOTNUMIN),(5,0(R3)),ALIGN=LEFT                               
         CLI   0(R3),C' '                                                       
         BE    *+12                                                             
         LA    R3,1(R3)                                                         
         B     *-12                                                             
         MVI   0(R3),X'5E'         NO INVOICE CONFIRMED COST                    
         LA    R3,1(R3)                                                         
         EDIT  (P11,GRSTOTAL),(11,0(R3)),ALIGN=LEFT,FLOAT=-,           X        
               ZERO=NOBLANK                                                     
         CLI   0(R3),C' '                                                       
         BE    *+12                                                             
         LA    R3,1(R3)                                                         
         B     *-12                                                             
*                                                                               
         MVI   0(R3),X'15'                                                      
         LA    R3,1(R3)                                                         
*                                                                               
         LA    R1,OUTAREA                                                       
         SR    R3,R1                                                            
         STCM  R3,3,OUTRECLN                                                    
         LA    R3,OUTAREA                                                       
         L     R0,AOUTFILE                                                      
         PUT   (R0),(R3)                                                        
*                                                                               
PRTR40   MVC   P1(50),=50C'*'                                                   
         MVC   P2(15),=CL15'TOTAL NUMBER OF'                                    
         MVC   P2+16(7),=C'SPOTS ='                                             
         EDIT  (B4,TOTNUMSP),(5,P2+27),ALIGN=LEFT                               
         MVC   P3(27),=CL27'TOTAL NUMBER OF INVOICES = '                        
         EDIT  (B4,TOTNUMIN),(5,P3+27),ALIGN=LEFT                               
         MVC   P4+5(22),=CL22'TOTAL DOLLAR AMOUNT = '                           
         EDIT  (P11,GRSTOTAL),(11,P4+27),2,ALIGN=LEFT,FLOAT=-,         X        
               ZERO=NOBLANK,COMMAS=YES                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         L     R1,ATWA                                                          
         L     RF,TWADCONS-T230FFD(,R1)                                         
         L     RE,TSPFUSER-TWADCOND(,RF) GET DCB ADDRESS                        
         ST    RE,AOUTFILE                                                      
*                                                                               
         MVC   128(4,RE),OURINVN   SAVE LAST REQ INVOICE #                      
*                                                                               
PRX      DELETE EP=T23022                                                       
         LTR   RF,RF                                                            
         BZ    XIT                                                              
         DC    H'0'                DELETE AFTER LOAD                            
         SPACE 2                                                                
***********************************************************************         
* CHECK TO SEE IF WE HAVE TO SEND OUT ANY ITEMS                                 
***********************************************************************         
ANYITMS  LR    R0,RE                                                            
         OC    NUMBITMS,NUMBITMS   ANY INVOICE ITEMS TO SHOW?                   
         BZR   RE                   NO                                          
*                                                                               
         BAS   RE,INITSPT          SET TO SPOT                                  
*                                                                               
         MVC   BIGKEY(L'KEY),KEY                                                
         GOTO1 =A(SENDTHEM)        YES                                          
         MVC   KEY,BIGKEY                                                       
*                                                                               
         CLC   =X'0E03',KEY        THIS XSP                                     
         BNE   *+8                                                              
         BAS   RE,INITXSP          SET TO XSP                                   
*                                                                               
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE PUTS THE INVOICE ITEMS FOR THIS INVOICE RECORD INTO THE          
* BUFFER                                                                        
*                                                                               
* ON ENTRY:    ANXTITMS            A(NEXT ENTRY FOR INVOICE ITEM TABLE)         
*              AIO                 A(INVOICE RECORD)                            
***********************************************************************         
PUTINBUF NTR1                                                                   
         LA    R4,KEY                                                           
         USING INVKEY,R4                                                        
*                                                                               
         NI    BITFLAG2,X'FF'-BTITMUSD      NO ITEMS USED YET                   
*                                                                               
         CLC   LSTINVKY(INVKSQ-INVKEY),KEY  SAME BASE KEY?                      
         BNE   PBUF10              NO                                           
*                                                                               
         ZIC   R1,LSTINVKY+INVKSQ-INVKEY   YES, SEQ IN THE SAME DECADE?         
         CVD   R1,DUB                                                           
         MVC   BYTE,DUB+6                                                       
         ZIC   R1,INVKSQ                                                        
         CVD   R1,DUB                                                           
         CLC   BYTE,DUB+6                                                       
         BE    PBUF20              YES, SAME DECADE, SAME FILMS & INV #         
*                                                                               
PBUF10   MVC   LSTINVKY,KEY                                                     
         XC    SNTINOSQ,SNTINOSQ   ASSUME NO INVOICE # FIRST                    
         BAS   RE,COPYINFO         COPY INFO JUST IN CASE OF SEQ                
         DROP  R4                                                               
*                                                                               
PBUF20   L     R4,ANXTITMS                                                      
         USING ITMSDSCT,R4                                                      
*                                                                               
* DON'T CHECK GENERATION ELEMENT BECAUSE NOT ALL ITEMS MAY BE CONVERTED         
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,IWRKRCDQ     GOT WRKR GENERATION ALREADY? (08)            
         BAS   RE,GETEL                                                         
         BNE   PBUF22                                                           
*                                                                               
         USING IWKRELEM,R6                                                      
         CLI   REQRRUNH+5,0        YES, IS THIS A RERUN REQUEST                 
         BE    PBUF24               NO, CK FOR UNCONVERTED                      
*                                                                               
         CLC   RRUNDATE,IWRKRDTE   SAME DATE AS RERUN DATE?                     
         BNE   PBUFXIT              NO, SKIP THIS RECORD                        
         B     PBUF24                                                           
         DROP  R6                                                               
*                                                                               
PBUF22   CLI   REQRRUNH+5,0        SHOULD THERE A WORKER GEN ELEMENT?           
         BNE   PBUFXIT              YES, BYPASS UNCONVERTED                     
*                                                                               
PBUF24   L     R6,AIO                                                           
         MVI   ELCODE,X'B1'        LOOK FOR AN INVOICE ITEM ELEMENT             
         BAS   RE,GETEL                                                         
*                                                                               
PBUF30   CLI   0(R6),X'B1'         STILL AN INVOICE ITEM?                       
         BNE   PBUFX               NO, SORT THE INVOICE ITEMS                   
*                                                                               
         USING INVELEM,R6                                                       
*                                                                               
         XC    ELEM,ELEM                                                        
         MVC   ELEM(17),INVELEM                                                 
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    *+12                                                             
         CLI   TWAOFFC,C'*'        THIS A DDS TERMINAL                          
         BNE   PBUF31                                                           
         CLI   REQTRCE,C'Y'                                                     
         BNE   PBUF31                                                           
*                                                                               
         MVC   P(14),=CL14'INVOICE ITEM: '                                      
         GOTO1 HEXOUT,DMCB,ELEM,P+15,17                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PBUF31   CLI   REQRRUNH+5,0        IS THIS A RERUN?                             
         BE    PBUF32               NO                                          
         TM    INVSTAT2,X'40'      IS THIS ITEM MARKED?                         
         BZ    PBUFNEXT             NO, SKIP THIS ITEM                          
         B     PBUF34                                                           
*                                                                               
PBUF32   TM    INVSTAT2,X'40'      IS THIS ITEM MARKED?                         
         BO    PBUFNEXT             YES, SKIP THIS ITEM                         
*                                                                               
PBUF34   GOTO1 DATCON,DMCB,(2,INVDAT),(0,ADATE)                                 
         GOTO1 AGTBROAD,(R1),(1,ADATE),BRDAREA,GETDAY,ADDAY                     
*                                                                               
         CLC   BRDAREA,KEYBROAD    MAKE SURE DETAIL IS IN RECORD'S              
         BE    PBUF35                  BROADCAST MONTH                          
*                                                                               
         BAS   RE,BADIITEM         BAD INVOICE ITEM                             
         MVC   P2+4(38),=CL38'THAT DOES NOT BELONG IN THE BROADCAST '           
         MVC   P2+42(9),=CL9'MONTH OF '                                         
         GOTO1 DATCON,DMCB,(0,KEYBRDST),(11,P2+51)                              
         MVI   P2+60,C'-'                                                       
         GOTO1 (RF),(R1),(0,KEYBRDND),(11,P2+62)                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PBUFNEXT            GET NEXT DETAIL                              
*                                                                               
BADIITEM MVC   P(39),=CL39'THIS RECORD HAS AN INVOICE ITEM DATED: '             
         LR    R0,RE                                                            
         GOTO1 DATCON,DMCB,(2,INVDAT),(11,P+39)                                 
         LR    RE,R0                                                            
         MVC   P+48(11),=CL11'AND TIMED: '                                      
         EDIT  (B2,INVTIM),(4,P+59),FILL=0                                      
         BR    RE                                                               
*                                                                               
PBUF35   MVC   ITMSINSQ,SNTINOSQ                                                
*                                                                               
         TM    FILTFLAG,FILTPRDQ                                                
         BZ    *+14                                                             
         CLC   INVPRD,BPRD         FITS PRODUCT FILTER?                         
         BNE   PBUFNEXT            NO                                           
*                                                                               
         MVC   ITMSPROD,INVPRD                                                  
         MVC   ITMSDATE,INVDAT                                                  
         MVC   ITMSTIME,INVTIM                                                  
         MVC   ITMSSPLN,INVLEN                                                  
*                                                                               
         TM    INVSTAT,X'40'       ESTIMATE IN THE PIGGYBACK                    
         BNZ   PBUF40                                                           
         OC    INVPRD2,INVPRD2     NO, DIE IF THERE IS A PIGGYBACK              
         BZ    PBUF40                                                           
*                                                                               
         BAS   RE,BADIITEM                                                      
         MVC   P2+4(16),=CL16'WITH A PIGGYBACK'                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PBUFNEXT            GET NEXT DETAIL                              
*                                                                               
PBUF40   CLI   INVPRD2,0           MAKE SURE IT'S NOT A ZERO ESTIMATE           
         BNE   PBUF42                                                           
*                                                                               
         CLC   AGENCY,=C'LI'       ALLOW LIPA TO BYPASS ESTIMATE CK             
         BE    PBUF42                                                           
         CLC   AGENCY,=C'AP'       ALLOW APPLE TO BYPASS ESTIMATE CK            
         BE    PBUF42                                                           
*                                                                               
         BAS   RE,BADIITEM                                                      
         MVC   P2+4(25),=CL25'THAT HAS AN ESTIMATE OF 0'                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PBUFNEXT            GET NEXT DETAIL                              
*                                                                               
PBUF42   TM    FILTFLAG,FILTESTQ                                                
         BZ    *+14                                                             
         CLC   BEST,INVPRD2        FITS ESTIMATE FILTER?                        
         BNE   PBUFNEXT            NO                                           
*                                                                               
         MVC   ITMSESTM,INVPRD2    COPY THE ESTIMATE                            
*                                                                               
         GOTO1 =A(GETIDREC)        GET ID RECORD FROM ESTIMATE FILTER           
         BNE   PBUFNEXT                                                         
*                                                                               
         CLI   EZPROF07,C'Y'       FORCE ZERO COST                              
         BE    PBUF46                                                           
         SPACE                                                                  
         MVC   ITMSCOST+1(L'INVCOST),INVCOST                                    
         MVC   ITMSCOST(L'INVCOSTX),INVCOSTX                                    
         SPACE                                                                  
PBUF46   MVC   ITMSFILM,INVFILM                                                 
*                                                                               
         CLI   INVFILM,0                                                        
         BE    PBUF50                                                           
         SPACE                                                                  
         GOTO1 =A(CKFILMTB),RR=RELO    CHECK OUR SAVED FILM TABLE               
         SPACE                                                                  
PBUF50   LA    R4,ITMSNEXT         SAVE A(NEXT ENTRY FOR INVOICE ITEM)          
         ST    R4,ANXTITMS                                                      
*                                                                               
         OI    BITFLAG2,BTITMUSD   AN ITEM WAS USED                             
         OI    INVSTAT2,X'40'      MARK THE ITEM USED                           
*                                                                               
         L     R1,NUMBITMS         INCREMENT NUMBER OF INVOICE ITEMS            
         LA    R1,1(,R1)                                                        
         ST    R1,NUMBITMS                                                      
*                                                                               
PBUFNEXT MVI   ELCODE,X'B1'                                                     
         BAS   RE,NEXTEL                                                        
         B     PBUF30                                                           
*                                                                               
PBUFX    L     R6,AIO              RESTORE BLOCK                                
         XC    KEY,KEY                                                          
         MVC   KEY(L'INVKEY),0(R6)                                              
         MVC   FILENAME,=CL8'SPTDIR'                                            
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
*                                                                               
         TM    BITFLAG2,BTITMUSD   PUT OUT WORKER GENERATION ELEMENT?           
         BZ    XIT                 NO                                           
*                                                                               
         MVC   AIO,AIO2            SO WE CAN USE OUR RECORD                     
         MVI   RDUPDATE,C'Y'                                                    
         MVC   FILENAME,=CL8'SPTFIL'                                            
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
         MVC   AIO,AIO1                                                         
*                                                                               
         MVI   ELCODE,IWRKRCDQ     GOT WRKR GENERATION ALREADY? (08)            
         BAS   RE,GETEL                                                         
         BE    PBUFX10             YES, DON'T BOTHER, BUT PUT ANYWAY            
*                                                                               
         L     R6,AIO                                                           
         XC    ELEM,ELEM           CREATE WORKER GENERATION ELEMENT             
         LA    R3,ELEM                                                          
         USING IWKRELEM,R3                                                      
         MVI   IWRKRCDE,IWRKRCDQ   SET AS 08                                    
         MVI   IWRKRLEN,IWRKRLNQ                                                
         GOTO1 DATCON,DMCB,(5,0),(3,IWRKRDTE)                                   
         DROP  R3                                                               
*                                                                               
         GOTO1 ADDELEM             ADD WORKER GENERATION ELEMENT                
         SPACE                                                                  
         CLC   13(2,R6),=H'1975'   EXCEEDED MAX SPOT REC LENGTH?                
         BNH   *+6                                                              
         DC    H'0'                MUST GET RTN FOR CREATING NEW REC            
*                                                                               
PBUFX10  MVC   FILENAME,=CL8'SPTFIL'                                            
         GOTO1 PUTREC                                                           
         XC    FILENAME,FILENAME                                                
PBUFXIT  B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE PUTS THE INVOICE ITEMS FOR THIS NEW INVOICE REC INTO THE         
* BUFFER                                                                        
*                                                                               
* ON ENTRY:    ANXTITMS            A(NEXT ENTRY FOR INVOICE ITEM TABLE)         
*              AIO                 A(INVOICE RECORD)                            
***********************************************************************         
PUTINBNW NTR1                                                                   
         LA    R4,KEY                                                           
         USING SNVKEYD,R4                                                       
*                                                                               
         NI    BITFLAG2,X'FF'-BTITMUSD   NO ITEMS USED YET                      
*                                                                               
         CLC   LSTINVKY(L'SNVKMAST),KEY  SAME BASE KEY?                         
         BE    PBNW20                     YES                                   
*                                                                               
         MVC   LSTINVKY,KEY                                                     
         XC    SNTINOSQ,SNTINOSQ   ASSUME NO INVOICE # FIRST                    
         BAS   RE,COPYINFN         COPY INFO JUST IN CASE OF SEQ                
         DROP  R4                                                               
*                                                                               
PBNW20   L     R4,ANXTITMS                                                      
         USING ITMSDSCT,R4                                                      
*                                                                               
* MUST CHECK GENERATION ELEMENT FOR PRD/PTR/EST                                 
* IF ALL ARE ZERO, WILL BE IN DETAIL ELEMENTS                                   
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,SNVHDELQ     CK IF CONVERTED DATE                         
         BAS   RE,GETEL                                                         
         BNE   PBNW22                                                           
*                                                                               
         USING SNVHDELD,R6                                                      
         MVC   SVSNVPRD,SNVHDPRD                                                
         MVC   SVSNVPR2,SNVHDPR2                                                
         MVC   SVSNVEST,SNVHDEST                                                
         MVC   SVSNVSDT,SNVHDSDT                                                
*                                                                               
         OC    SNVHDEZD,SNVHDEZD   WAS THIS DUMPED ALREADY                      
         BZ    PBNW22               NO                                          
         SPACE                                                                  
         CLI   REQRRUNH+5,0        YES, IS THIS A RERUN REQUEST                 
         BE    PBNW24               NO, CK FOR UNCONVERTED                      
*                                                                               
         CLC   RRUNDATE,SNVHDEZD   SAME DATE AS RERUN DATE?                     
         BNE   PBNWXIT              NO, SKIP THIS RECORD                        
         B     PBNW24                                                           
         DROP  R6                                                               
*                                                                               
PBNW22   CLI   REQRRUNH+5,0        SHOULD THERE A WORKER GEN ELEMENT?           
         BNE   PBNWXIT              YES, BYPASS UNCONVERTED                     
*                                                                               
PBNW24   L     R6,AIO                                                           
         MVI   ELCODE,SNVIDELQ     LOOK FOR AN INVOICE ITEM ELEMENT             
         BAS   RE,GETEL                                                         
*                                                                               
PBNW30   CLI   0(R6),SNVIDELQ      STILL AN INVOICE ITEM?                       
         BNE   PBNWX               NO, SORT THE INVOICE ITEMS                   
*                                                                               
         USING SNVIDELD,R6                                                      
*                                                                               
         XC    ELEM,ELEM                                                        
         MVC   ELEM(SNVIDLNQ),0(R6)                                             
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    *+12                                                             
         CLI   TWAOFFC,C'*'        THIS A DDS TERMINAL                          
         BNE   PBNW31                                                           
         CLI   REQTRCE,C'Y'                                                     
         BNE   PBNW31                                                           
*                                                                               
         MVC   P(14),=CL14'INVOICE ITEM: '                                      
         GOTO1 HEXOUT,DMCB,ELEM,P+15,33                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PBNW31   CLI   REQRRUNH+5,0        IS THIS A RERUN?                             
         BE    PBNW32               NO                                          
         TM    SNVIDCTL,SNVIDEZQ   IS THIS ITEM MARKED?                         
         BZ    PBNWNEXT             NO, SKIP THIS ITEM                          
         B     PBNW34                                                           
*                                                                               
PBNW32   TM    SNVIDCTL,SNVIDEZQ   IS THIS ITEM MARKED?                         
         BO    PBNWNEXT             YES, SKIP THIS ITEM                         
*                                                                               
PBNW34   DS    0H                                                               
*        MVC   HALF,SNVKMOS-SNVKEYD+KEY                                         
         MVC   HALF,SVSNVSDT      USE INVOICE START DATE, NOT MOS !!!           
*        XC    HALF,=X'FFFF'                                                    
         GOTO1 DATCON,DMCB,(2,HALF),(0,ADATE)                                   
*        GOTO1 AGTBROAD,(R1),(1,ADATE),BRDAREA,GETDAY,ADDAY                     
*                                                                               
         ZIC   R0,SNVIDDAY                                                      
*        GOTO1 ADDAY,DMCB,BRDAREA,PARAS,(R0)                                    
         GOTO1 ADDAY,DMCB,ADATE,PARAS,(R0)                                      
*                                                                               
         CLC   PARAS(6),KEYBRDST   MAKE SURE DETAIL IS IN RECORD'S              
         BL    *+14                    BROADCAST MONTH                          
         CLC   PARAS(6),KEYBRDND   MAKE SURE DETAIL IS IN RECORD'S              
         BNH   PBNW35                  BROADCAST MONTH                          
*                                                                               
         BAS   RE,BADIITM          BAD INVOICE ITEM                             
         MVC   P2+4(38),=CL38'THAT DOES NOT BELONG IN THE BROADCAST '           
         MVC   P2+42(9),=CL9'MONTH OF '                                         
         GOTO1 DATCON,DMCB,(0,KEYBRDST),(11,P2+51)                              
         MVI   P2+60,C'-'                                                       
         GOTO1 (RF),(R1),(0,KEYBRDND),(11,P2+62)                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PBNWNEXT            GET NEXT DETAIL                              
*                                                                               
BADIITM  MVC   P(39),=CL39'THIS RECORD HAS AN INVOICE ITEM DATED: '             
         LR    R0,RE                                                            
         XC    FULL(3),FULL                                                     
         MVC   FULL+3(1),SNVIDDAY                                               
         MVC   DUB(6),KEYBRDST                                                  
         GOTO1 ADDAY,DMCB,DUB,FULL,DUB                                          
         GOTO1 DATCON,(R1),(0,DUB),(11,P+39)                                    
         LR    RE,R0                                                            
         MVC   P+48(11),=CL11'AND TIMED: '                                      
         EDIT  (B2,SNVIDTIM),(4,P+59),FILL=0                                    
         BR    RE                                                               
*                                                                               
PBNW35   MVC   ITMSINSQ,SNTINOSQ                                                
*                                                                               
         TM    FILTFLAG,FILTPRDQ                                                
         BZ    PBNW38                                                           
         CLC   SNVIDPRD,BPRD       FITS PRODUCT FILTER?                         
         BE    PBNW38                                                           
         CLI   SNVIDPRD,0          IS THERE A PRODUCT?                          
         BNE   PBNWNEXT             YES                                         
         CLC   SVSNVPRD,BPRD       FITS PRODUCT FILTER?                         
         BNE   PBNWNEXT             NO                                          
*                                                                               
PBNW38   TM    FILTFLAG,FILTESTQ                                                
         BZ    PBNW40                                                           
         CLC   BEST,SNVIDEST       FITS ESTIMATE FILTER?                        
         BE    PBNW40               YES                                         
         CLI   SNVIDEST,0          IS THERE A ESTIMATE                          
         BNE   PBNWNEXT             YES                                         
         CLC   SVSNVEST,BEST       FITS ESTIMATE FILTER?                        
         BNE   PBNWNEXT             NO                                          
*                                                                               
PBNW40   MVC   ITMSPROD,SNVIDPRD                                                
         MVC   ITMSESTM,SNVIDEST   COPY THE ESTIMATE                            
*                                                                               
         CLI   SNVIDPRD,0          IT PRODUCT IN DETAIL ELEMENT                 
         BNE   PBNW42               YES                                         
*                                                                               
         MVC   ITMSPROD,SVSNVPRD                                                
         MVC   ITMSESTM,SVSNVEST   COPY THE ESTIMATE                            
*                                                                               
PBNW42   GOTO1 DATCON,DMCB,(0,PARAS),(2,ITMSDATE)                               
         SPACE                                                                  
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,3,SNVIDTIM                                                    
         D     R0,=F'60'                                                        
         MH    R1,=H'100'                                                       
         AR    R1,R0                                                            
         AHI   R1,600              THE TIME IS DISPLACEMENT FROM 6:00A          
         STCM  R1,3,ITMSTIME                                                    
         SPACE                                                                  
         MVC   ITMSSPLN,SNVIDSLN                                                
*                                                                               
         MVC   FULL(1),KEY+SNVKAM-SNVKEYD    USE THE AGENCY/MEDIA               
         MVC   FULL+1(2),KEY+SNVKCLT-SNVKEYD & CLIENT FROM INVOICE REC          
*                                                                               
         BAS   RE,INITSPT                                                       
         MVC   BIGKEY,KEY                                                       
*                                                                               
         GOTO1 =A(GETIDREC)        GET ID RECORD FROM ESTIMATE FILTER           
         BAS   RE,INITXSP                                                       
         MVC   KEY,BIGKEY                                                       
         BNE   PBNWNEXT                                                         
*                                                                               
         CLI   EZPROF07,C'Y'       FORCE ZERO COST                              
         BE    PBNW46                                                           
         SPACE                                                                  
         MVC   ITMSCOST,SNVIDCST                                                
         SPACE                                                                  
PBNW46   MVC   ITMSFILM,SNVIDCML                                                
*                                                                               
         CLI   SNVIDCML,0                                                       
         BE    PBNW50                                                           
         SPACE                                                                  
         GOTO1 =A(CKFILMTB),RR=RELO    CHECK OUR SAVED FILM TABLE               
         SPACE                                                                  
PBNW50   LA    R4,ITMSNEXT         SAVE A(NEXT ENTRY FOR INVOICE ITEM)          
         ST    R4,ANXTITMS                                                      
*                                                                               
         OI    BITFLAG2,BTITMUSD   AN ITEM WAS USED                             
         OI    SNVIDCTL,SNVIDEZQ   MARK THE ITEM USED                           
*                                                                               
         L     R1,NUMBITMS         INCREMENT NUMBER OF INVOICE ITEMS            
         LA    R1,1(,R1)                                                        
         ST    R1,NUMBITMS                                                      
*                                                                               
PBNWNEXT MVI   ELCODE,SNVIDELQ     LOOK FOR AN INVOICE ITEM ELEMENT             
         BAS   RE,NEXTEL                                                        
         B     PBNW30                                                           
*                                                                               
PBNWX    L     R6,AIO              RESTORE BLOCK                                
         XC    KEY,KEY                                                          
         MVC   KEY(L'SNVKEY),0(R6)                                              
         MVC   FILENAME,=CL8'XSPDIR'                                            
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
         CLC   KEY(L'SNVKEY),0(R6)                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    BITFLAG2,X'80'      PUT OUT WORKER GENERATION ELEMENT?           
         BZ    XIT                 NO                                           
*                                                                               
         MVC   AIO,AIO2            SO WE CAN USE OUR RECORD                     
         MVI   RDUPDATE,C'Y'                                                    
         MVC   FILENAME,=CL8'XSPFIL'                                            
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
* ONLY CHECK IF THIS IS FIRST OR ONLY RECORD FOR THIS INVOICE                   
*                                                                               
         TM    BITFLAG2,BTITMULT   IF MULTI RECORD SET                          
         BO    PBNW80               NO HEADER ELEM TO UPDATE                    
*                                                                               
         MVI   ELCODE,SNVHDELQ     BETTER HAVE HEADER ELEM                      
         BAS   RE,GETEL                                                         
         BE    *+6                  YES                                         
         DC    H'0'                                                             
         USING SNVHDELD,R6                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(3,SNVHDEZD)                                   
*                                                                               
PBNW80   GOTO1 PUTREC                                                           
         XC    FILENAME,FILENAME                                                
*                                                                               
PBNWXIT  DS   0H                                                                
         B     XIT                                                              
         DROP  R4,R6                                                            
INITSPT  MVC   LKEY,=Y(L'AGYKEY)                                                
         MVC   LSTATUS,=Y(L'AGYCTL)                                             
         MVC   DATADISP,=Y(AGYEL-AGYHDR)                                        
         BR    RE                                                               
INITXSP  MVC   LKEY,=Y(L'SNVKEY)                                                
         MVC   LSTATUS,=Y(L'SNVDSTAT)                                           
         MVC   DATADISP,=Y(SNVELS-SNVKEYD)                                      
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE GET INFORMATION FROM THIS RECORD JUST IN CASE THERE              
* ARE ANY INVOICES WITH A SEQUENCE NUMBER IN THE "NEW POSITION".                
* THE FILM CODES AND THE INVOICE # OF THE BASE INVOICE (BEGINNING OF            
* THE DECADE) ARE SHARED BY THE INVOICES IN THE SAME DECADE.                    
***********************************************************************         
COPYINFO NTR1                                                                   
         L     R6,AIO              IF WE HAVE AN INVOICE HEADER ELEM            
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   CINF10                                                           
         BAS   RE,ADINVNTB         THEN WE USE THE INVOICE # FROM IT            
         B     CINF20                                                           
*                                                                               
CINF10   L     R6,AIO              ELSE WE GET THE INVOICE # FROM HERE          
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   CINF20                                                           
         BAS   RE,ADINVNTB                                                      
*                                                                               
CINF20   L     R2,ASFLMTBL                                                      
         USING FILMDSCT,R2                                                      
         LR    R0,R2               CLEAR THE SAVED FILM TABLE                   
         LA    R1,LSFLMTBL                                                      
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R6,AIO              SAVE THE FILM TABLE FOR THIS BASE            
         MVI   ELCODE,X'04'            INVOICE                                  
         BAS   RE,GETEL                                                         
*                                                                               
CINF20LP CLI   0(R6),X'04'         STILL A FILM CODE TRANSLATION ELEM?          
         BNE   CINFX               NO, DONE                                     
*                                                                               
         USING INVFLMEL,R6                                                      
         MVC   FILMNTRY,INVFID                                                  
         LA    R2,FILMNEXT                                                      
         BAS   RE,NEXTEL                                                        
         B     CINF20LP                                                         
*                                                                               
CINFX    B     XIT                                                              
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE GET INFORMATION FROM THE NEW INVOICE RECORD                      
***********************************************************************         
COPYINFN NTR1                                                                   
         L     R6,AIO              IF WE HAVE AN INVOICE HEADER ELEM            
         L     R3,ANXTINVN                                                      
         USING INVNDSCT,R3                                                      
*                                                                               
         MVC   INVNNUMB,LSTINVKY+SNVKINV-SNVKEYD                                
*                                                                               
         ZICM  R1,NUMBINVN,2       SEQUENCE # FOR THIS INVOICE                  
         LA    R1,1(,R1)                                                        
         STCM  R1,3,NUMBINVN                                                    
         MVC   INVNSEQN,NUMBINVN                                                
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    *+12                                                             
         CLI   TWAOFFC,C'*'        THIS A DDS TERMINAL                          
         BNE   CINFN10                                                          
         CLI   REQTRCE,C'Y'                                                     
         BNE   CINFN10                                                          
*                                                                               
         MVC   P(16),=CL16'INVOICE NUMBER: '                                    
         MVC   P+16(L'INVNNUMB),INVNNUMB                                        
         MVC   P+28(16),=CL16'HAS A SEQ # OF: '                                 
         GOTO1 HEXOUT,DMCB,NUMBINVN,P+44,2                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
CINFN10  LA    R3,INVNNEXT                                                      
         ST    R3,ANXTINVN         SAVE A(NEXT INVOICE NUMBER ENTRY)            
         DROP  R3                                                               
*                                                                               
         MVC   SNTINOSQ,NUMBINVN                                                
*                                                                               
CINFN20  L     R2,ASFLMTBL                                                      
         USING FILMDSCT,R2                                                      
         LR    R0,R2               CLEAR THE SAVED FILM TABLE                   
         LA    R1,LSFLMTBL                                                      
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R6,AIO              SAVE THE FILM TABLE FOR THIS BASE            
         MVI   ELCODE,SNVCMELQ         INVOICE                                  
         BAS   RE,GETEL                                                         
*                                                                               
CINFN20L CLI  0(R6),SNVCMELQ      STILL A FILM CODE TRANSLATION ELEM?           
         BNE   CINFNX              NO, DONE                                     
*                                                                               
         USING SNVCMELD,R6                                                      
         MVC   FILMIDCD,SNVCMICD                                                
         MVC   FILMCODE,SNVCMCD                                                 
         LA    R2,FILMNEXT                                                      
         BAS   RE,NEXTEL                                                        
         B     CINFN20L                                                         
*                                                                               
CINFNX   B     XIT                                                              
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* ADDS THE INVOICE NUMBER TO OUR INVOICE TABLE AND GIVES A SEQUENCE             
* NUMBER TO THIS TABLE                                                          
*                                                                               
* ON ENTRY:    (R6)                A(ID TRANSLATION ELEM)                       
*                                                                               
* ON EXIT:     SNTINOSQ            SEQ NUMBER TO OUR INVOICE # TABLE            
***********************************************************************         
ADINVNTB NTR1                                                                   
         L     R3,ANXTINVN                                                      
         USING INVNDSCT,R3                                                      
*                                                                               
         CLI   0(R6),X'05'         INVOICE NUMBER FROM INVOICE HDR              
         BNE   AINVN10                                                          
         USING IHDELEM,R6                                                       
         MVC   INVNNUMB,IHDINV                                                  
         B     AINVN20                                                          
*                                                                               
         USING INVIDEL,R6                                                       
AINVN10  MVC   INVNNUMB,INVID      DEFAULT IS TO CUT OFF LAST 2 BYTES           
         CLC   INVINO(2),SPACES                                                 
         BNE   *+10                                                             
         MVC   INVNNUMB,INVID+2    SPACES IN FRONT, CUT 2 OFF BEGINNING         
         DROP  R6                                                               
*                                                                               
AINVN20  ZICM  R1,NUMBINVN,2       SEQUENCE # FOR THIS INVOICE                  
         LA    R1,1(,R1)                                                        
         STCM  R1,3,NUMBINVN                                                    
         MVC   INVNSEQN,NUMBINVN                                                
*                                                                               
*         MVC   P(16),=CL16'INVOICE NUMBER: '                                   
*         MVC   P+16(L'INVNNUMB),INVNNUMB                                       
*         MVC   P+28(16),=CL16'HAS A SEQ # OF: '                                
*         GOTO1 HEXOUT,DMCB,NUMBINVN,P+44,2                                     
*         GOTO1 SPOOL,DMCB,(R8)                                                 
**                                                                              
         LA    R3,INVNNEXT                                                      
         ST    R3,ANXTINVN         SAVE A(NEXT INVOICE NUMBER ENTRY)            
         DROP  R3                                                               
*                                                                               
         MVC   SNTINOSQ,NUMBINVN                                                
*                                                                               
AINVNX   B     XIT                                                              
         EJECT                                                                  
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
MEDIALST DC    C'TRNX'             MEDIA LIST                                   
***********************************************************************         
* ERROR MESSAGES                                                                
***********************************************************************         
         SPACE                                                                  
         SPACE                                                                  
INVLFLD  MVI   ERROR,INVALID                                                    
         B     ERREXIT                                                          
*                                                                               
MISSMED  LA    R2,REQMEDH                                                       
         B     MISSFLD                                                          
MISSCLT  LA    R2,REQCLTH                                                       
         B     MISSFLD                                                          
MISSPRD  LA    R2,REQPRDH                                                       
*                                                                               
MISSFLD  MVI   ERROR,MISSING                                                    
*                                                                               
ERREXIT  GOTO1 ERREX                                                            
*                                                                               
BADAGYCD XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADAGYMS),BADAGYMS                                     
         MVC   CONHEAD+47(2),SVESTAGY                                           
         B     ERREXIT2                                                         
BADDATE  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADATEMS),BADATEMS                                     
*                                                                               
ERREXIT2 GOTO1 ERREX2                                                           
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         LTORG                                                                  
*&&DO                                                                           
APPC     DC    CL5'AP'                                                          
BLPC     DC    CL5'BL'                                                          
IRPC     DC    CL5'IR'                                                          
K3PC     DC    CL5'K3'                                                          
LIPC     DC    CL5'LI'                                                          
RAPC     DC    CL5'RA'            RMRNY REGIONAL MARKET RADIO                   
PTPC     DC    CL5'V8'                                                          
*&&                                                                             
BADAGYMS DC    C'* ERROR * AGENCY CODE DIFFERENT FROM ESTIMATE (  ) *'          
BADATEMS DC    C'* ERROR * ENTER MONTH YEAR FOR BROADCAST MONTH *'              
OUTFILE  DCB   DDNAME=OUTFILE,MACRF=PM,DSORG=PS,RECFM=VB,              X        
               BLKSIZE=8200,LRECL=2048,BUFNO=2                                  
HEADING  SSPEC H1,3,C'EASI SYSTEM'                                              
         SSPEC H1,35,C'I N V O I C E  T R A N S F E R'                          
         SSPEC H1,73,AGYNAME                                                    
         SSPEC H2,35,C'------------------------------'                          
         SSPEC H2,73,AGYADD                                                     
         SSPEC H4,73,REPORT                                                     
         SSPEC H4,85,RUN                                                        
         SSPEC H5,73,REQUESTOR                                                  
         SSPEC H5,103,PAGE                                                      
         SSPEC H8,3,C'AGENCY'                                                   
         SSPEC H9,3,C'------'                                                   
         SSPEC H8,29,C'MEDIA'                                                   
         SSPEC H9,29,C'-----'                                                   
         SSPEC H8,37,C'STATION'                                                 
         SSPEC H9,37,C'-------'                                                 
         SSPEC H8,47,C'CLIENT'                                                  
         SSPEC H9,47,C'------'                                                  
         SSPEC H8,56,C'PRODUCT'                                                 
         SSPEC H9,56,C'-------'                                                 
         SSPEC H8,65,C'EST'                                                     
         SSPEC H9,65,C'---'                                                     
         SSPEC H8,72,C'INVOICE DATE'                                            
         SSPEC H9,72,C'------------'                                            
         SSPEC H8,88,C'ITEMS'                                                   
         SSPEC H9,88,C'-----'                                                   
         SSPEC H8,104,C'AMOUNT'                                                 
         SSPEC H9,104,C'------'                                                 
         DC    X'00'               END MARKER FOR SSPECS                        
         DROP  R7,RB,RC                                                         
         EJECT                                                                  
***********************************************************************         
* CHECKS IF WE HAVE THIS FILM CODE ALREADY, ADD IT IF NOT                       
*                                                                               
* ON EXIT:     ITMSFILM            CHANGED TO OUR INTERNAL FILM CODE            
***********************************************************************         
         SPACE                                                                  
CKFILMTB NMOD1 0,**+CKF**                                                       
         L     RC,SVRC                                                          
         USING GEND,RC                                                          
         L     R3,ASFLMTBL                                                      
         USING FILMDSCT,R3                                                      
         L     R4,ANXTITMS                                                      
         USING ITMSDSCT,R4                                                      
*                                                                               
CFLM10LP CLI   FILMIDCD,X'00'      NO FILM ENTRIES IN OUR SAVED TABLE?          
         BNE   CFLM10                                                           
         MVC   P(39),=CL39'THIS RECORD HAS A BAD FILM CODE IN THE '             
         MVC   P+39(20),=CL20'INVOICE ITEM DATED: '                             
         GOTO1 DATCON,DMCB,(2,ITMSDATE),(11,P+59)                               
         MVC   P+68(11),=CL11'AND TIMED: '                                      
         EDIT  (B2,ITMSTIME),(4,P+79)                                           
         MVC   P2+4(26),=CL26'FILM CODE HAS BEEN IGNORED'                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   ITMSFILM,0                                                       
         B     CFLMX                                                            
*                                                                               
CFLM10   CLC   FILMIDCD,ITMSFILM   FOUND A MATCH?                               
         BE    CFLM20                                                           
         LA    R3,FILMNEXT         NO, LOOK AT NEXT ENTRY                       
         B     CFLM10LP                                                         
         DROP  R3                                                               
*                                                                               
CFLM20   L     R2,AFILMTBL                                                      
         USING FILMDSCT,R2                                                      
*                                                                               
CFLM20LP CLI   FILMIDCD,X'FF'                                                   
         BNE   CFLM30                                                           
*                                                                               
         ZIC   R1,NUMBFILM         NEW FILM CODE                                
         LA    R1,1(R1)                                                         
         STC   R1,NUMBFILM                                                      
*                                                                               
         CLI   NUMBFILM,MAXFILMS   DIE IF OVERFLOW                              
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         STC   R1,FILMIDCD         ADD FILM CODE AND ID TO TABLE                
         MVC   FILMCODE,FILMCODE-FILMNTRY(R3)                                   
         STC   R1,ITMSFILM         INVOICE ITEM IN BUFFER GETS NEW ID           
         LA    R2,FILMNEXT                                                      
         MVI   FILMIDCD,X'FF'      MARK END OF TABLE                            
         B     CFLMX                                                            
*                                                                               
CFLM30   CLC   FILMCODE,FILMCODE-FILMNTRY(R3)                                   
         BNE   CFLM40                                                           
         MVC   ITMSFILM,FILMIDCD   INVOICE ITEM IN BUFFER GETS FILM ID          
         B     CFLMX                                                            
*                                                                               
CFLM40   LA    R2,FILMNEXT                                                      
         B     CFLM20LP                                                         
         DROP  R2,R4                                                            
*                                                                               
CFLMX    XIT1                                                                   
         LTORG                                                                  
         DROP  RB,RC                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE SENDS OUT THE INVOICE ITEMS                                      
***********************************************************************         
         SPACE                                                                  
SENDTHEM NMOD1 0,*SENDTH*                                                       
         L     RC,SVRC                                                          
         USING GEND,RC                                                          
         L     R3,NUMBITMS         NUMBER OF INVOICE ITEMS                      
         L     R4,AITMSTBL                                                      
         GOTO1 QSORT,DMCB,(R4),(R3),L'ITMSNTRY,L'ITMSKEY,0                      
******                                                                          
* SET UP OUR ITEMS TABLE                                                        
******                                                                          
         USING ITMSDSCT,R4                                                      
         L     R5,NUMBITMS         COUNTDOWN NUMBER OF INVOICE ITEMS            
         MVI   SNTPRDCD,0          NOTHING HAS BEEN SENT YET                    
         MVI   SNTESTCD,0                                                       
         MVC   SNTINOSQ,=X'FFFF'                                                
         NI    BITFLAG1,X'FF'-X'01'  DON'T PUT OUT INVOICE TOTAL REC            
*                                                                               
         SR    R6,R6               ZERO ITEMS PER INVOICE CT                    
******                                                                          
* WHERE TO BEGIN LOOPING                                                        
******                                                                          
STHMLOOP XC    ELEM,ELEM                                                        
         MVC   ELEM(L'ITMSNTRY),ITMSNTRY                                        
******                                                                          
* AGENCY RECORD                                                                 
******                                                                          
STHMAGY  CLC   SNTPOWER,ITMSAGPC   SAME AGENCY POWER CODE?                      
         BE    STHMSTA                                                          
*                                                                               
STHMAG20 MVC   SNTPOWER,ITMSAGPC                                                
         XC    OUTRECLN,OUTRECLN                                                
         LA    R3,OUTDATA                                                       
         MVC   0(2,R3),=C'21'      AGENCY RECORD CODE                           
         MVI   2(R3),X'5E'         SEMICOLONS USED TO SEPARATE FIELDS           
         MVI   3(R3),X'5E'         NO AGENCY ID                                 
         LA    R3,4(R3)                                                         
*                                                                               
         TM    BITFLAG1,X'08'      FILTER ON AGENCY?                            
         TM    FILTFLAG,FILTAGYQ   FILTER ON AGENCY                             
         BNZ   STHMAG30                                                         
         MVC   HALF,ITMSAGPC                                                    
         GOTO1 =A(GETAGYNM)        GET AGENCY NAME AND ADDRESS                  
*                                                                               
STHMAG30 MVC   0(L'AGYNM,R3),AGYNM   YES, WE GOT THE AGENCY NAME                
         MVI   L'AGYNM(R3),X'5E'                                                
         LA    R3,L'AGYNM+1(R3)                                                 
         MVC   0(L'AGYADRES,R3),AGYADRES    AND AGENCY ADDRESS                  
*                                                                               
         MVI   L'AGYADRES(R3),X'15'   RECORD TERMINATOR                         
         LA    R3,L'AGYADRES+1(R3)                                              
*                                                                               
         LA    R1,OUTAREA                                                       
         SR    R3,R1                                                            
         SPACE                                                                  
         CH    R3,MAXRECSZ         MAX REC LENGTH FOR NOW                       
         BNH   *+8                                                              
         BAS   RE,MAXRECLN         GO PRINT OUT ERROR                           
         SPACE                                                                  
         STCM  R3,3,OUTRECLN                                                    
         LA    R3,OUTAREA                                                       
         L     R0,AOUTFILE                                                      
         PUT   (R0),(R3)                                                        
*                                                                               
******                                                                          
* STATION RECORD                                                                
******                                                                          
STHMSTA  CLC   BAGYMD,SNTAGYMD     IF MEDIA DIFFERENT                           
         BNE   *+14                                                             
         CLC   BSTA,SNTSTATN       OR STATION DIFFERENT                         
         BE    STHMHDR                                                          
*                                                                               
STHMST20 MVC   SNTAGYMD,BAGYMD     THEN WE NEED A NEW STATION RECORD            
         MVC   SNTSTATN,BSTA                                                    
*                                                                               
         XC    OUTRECLN,OUTRECLN                                                
         LA    R3,OUTDATA                                                       
         MVC   0(2,R3),=C'22'      STATION RECORD CODE                          
         MVI   2(R3),X'5E'         SEMICOLONS USED TO SEPARATE FIELDS           
         LA    R3,3(R3)                                                         
         MVC   0(4,R3),QSTA        CALL LETTERS                                 
         MVI   4(R3),X'5E'                                                      
         LA    R3,5(R3)                                                         
         MVC   0(1,R3),QMED        MEDIA                                        
         LA    R3,1(R3)                                                         
*                                                                               
         CLI   QMED,C'T'           IF TELEVISION                                
         BE    STHMST30                                                         
         CLI   QMED,C'N'             OR NETWORK                                 
         BE    STHMST30            THEN NO RADIO BANDS                          
*                                                                               
         MVI   0(R3),X'5E'                                                      
         MVC   1(L'QMED,R3),QSTA+4      BAND                                    
         MVI   2(R3),C'M'                                                       
         LA    R3,3(R3)                                                         
*                                                                               
STHMST30 MVI   0(R3),X'15'                                                      
         LA    R3,1(R3)                                                         
*                                                                               
         LA    R1,OUTAREA                                                       
         SR    R3,R1                                                            
         SPACE                                                                  
         CH    R3,MAXRECSZ         MAX REC LENGTH FOR NOW                       
         BNH   *+8                                                              
         BAS   RE,MAXRECLN         GO PRINT OUT ERROR                           
         SPACE                                                                  
         STCM  R3,3,OUTRECLN                                                    
         LA    R3,OUTAREA                                                       
         L     R0,AOUTFILE                                                      
         PUT   (R0),(R3)                                                        
*                                                                               
******                                                                          
* INVOICE HEADER RECORD                                                         
******                                                                          
STHMHDR  CLC   ITMSINSQ,SNTINOSQ   IF INVOICE NUMBER,                           
         BNE   STHMHD30                                                         
         CLC   ITMSPROD,SNTPRDCD       PRODUCT,                                 
         BNE   STHMHD30                                                         
         CLC   ITMSESTM,SNTESTCD       AND ESTIMATE IS THE SAME AS LAST         
         BE    STHMBRD                                                          
*                                                                               
STHMHD30 TM    BITFLAG1,X'01'      PUT OUT INVOICE TOTAL?                       
         BZ    *+8                                                              
         BAS   RE,PUTINVTO         YES                                          
         OI    BITFLAG1,X'01'      CREATING A HEADER SO WE NEED TOTAL           
         SR    R6,R6               ZERO ITEMS PER INVOICE CT                    
*                                                                               
         ZAP   INVTOTAL,=P'0'      NO TOTAL FOR THIS INVOICE YET                
         L     R1,TOTNUMIN         INCREMENT TOTAL NUMBER OF INVOICES           
         LA    R1,1(,R1)                                                        
         ST    R1,TOTNUMIN                                                      
*                                                                               
         XC    OUTRECLN,OUTRECLN                                                
         LA    R3,OUTDATA                                                       
         MVC   0(2,R3),=C'31'      INVOICE HEADER CODE                          
         MVI   2(R3),X'5E'         SEMICOLONS USED TO SEPARATE FIELDS           
         LA    R3,3(R3)                                                         
*                                                                               
         MVC   0(25,R3),SGNONAGY   REPRESENTATIVE IS THE SIGN-ON AGENCY         
         MVI   25(R3),X'5E'                                                     
         LA    R3,26(R3)                                                        
*                                                                               
         MVI   0(R3),X'5E'         NO SALESPERSON                               
         LA    R3,1(R3)                                                         
*                                                                               
STHMHD40 MVC   0(25,R3),SPACES     CLEAR SPACE FOR CLIENT NAME                  
*                                                                               
         TM    BITFLAG1,X'40'      CLIENT SPECIFIED?                            
         BNZ   STHMHD50            YES, GOT THE INFO ALREADY                    
*                                                                               
         XC    FAKEFHDR(L'FAKEFHDR+L'FAKEFDAT),FAKEFHDR                         
         LA    R2,FAKEFHDR                                                      
         MVI   0(R2),L'FAKEFHDR+L'QCLT                                          
         MVC   2(2,R2),REQCLTH+2   COPY REQUEST CLIENT SCREEN POSITION          
         OI    4(R2),X'80'                                                      
         MVI   5(R2),3                                                          
         CLI   QCLT+2,C' '                                                      
         BNE   *+8                                                              
         MVI   5(R2),2                                                          
         MVC   8(L'QCLT,R2),QCLT                                                
         GOTO1 VALICLT                                                          
*                                                                               
STHMHD50 MVC   0(L'CLTNM,R3),CLTNM    CLIENT NAME                               
         MVI   25(R3),X'5E'                                                     
         LA    R3,26(R3)                                                        
*                                                                               
         MVC   0(25,R3),SPACES     CLEAR SPACE FOR PRODUCT NAME                 
*                                                                               
         CLC   ITMSPROD,SNTPRDCD   SAME PRODUCT AS LAST?                        
         BE    STHMHD70            THEN WE GOT THE NAME ALREADY                 
*                                                                               
         LA    R1,SVCLIST                                                       
         LHI   R0,255                                                           
STHMHD60 OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   ITMSPROD,3(R1)                                                   
         BE    STHMHD63                                                         
         LA    R1,4(R1)                                                         
         BCT   R0,STHMHD60                                                      
         DC    H'0'                                                             
*                                                                               
STHMHD63 MVC   QPRD,0(R1)                                                       
         XC    FAKEFHDR(L'FAKEFHDR+L'FAKEFDAT),FAKEFHDR                         
         LA    R2,FAKEFHDR                                                      
         MVI   0(R2),L'FAKEFHDR+L'QPRD                                          
         MVC   2(2,R2),REQCLTH+2   COPY REQUEST CLIENT SCREEN POSITION          
         OI    4(R2),X'80'                                                      
         MVI   5(R2),3                                                          
         CLI   QPRD+2,C' '                                                      
         BNE   *+8                                                              
         MVI   5(R2),2                                                          
         MVC   8(L'QPRD,R2),QPRD                                                
         GOTO1 VALIPRD                                                          
         MVC   PRDNM,WORK+4                                                     
*                                                                               
         XC    KEY,KEY             RESTORE THE READ BLOCK FOR SEQ               
         MVC   KEY,BIGKEY                                                       
         CLI   KEY,X'0B'           THIS AN OLD INVOICE                          
         BE    STHMHD66                                                         
         CLI   KEY,X'0E'           MUST BE A NEW INVOICE                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=CL8'XSPDIR'                                            
*                                                                               
STHMHD66 GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
         CLC   KEY(L'INVKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   KEY,X'0E'           THIS A NEW INVOICE                           
         BNE   STHMHD70                                                         
         CLC   KEY(L'SNVKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
STHMHD70 MVC   0(L'PRDNM,R3),PRDNM    PRODUCT NAME                              
         MVI   25(R3),X'5E'                                                     
         LA    R3,26(R3)                                                        
*                                                                               
         MVC   SNTINOSQ,ITMSINSQ                                                
         MVC   SNTPRDCD,ITMSPROD                                                
         MVC   SNTESTCD,ITMSESTM                                                
*                                                                               
         MVC   0(L'TODAYDTE,R3),TODAYDTE   OUR CREATED INVOICE DATE             
         MVI   6(R3),X'5E'                     IS TODAY'S DATE                  
         LA    R3,7(R3)                                                         
*                                                                               
         MVI   0(R3),X'5E'         NO ORDER TYPE                                
         LA    R3,1(R3)                                                         
* STEVE K. ASKED NOT TO SEND CLT/PRD/EST CODES JAN26/05                         
*        CLI   ITMSESTM,0          ANY ESTIMATE?                                
*        BNE   *+16                                                             
         MVI   0(R3),X'5E'         NONE                                         
         LA    R3,1(R3)                                                         
*        B     STHMHD80                                                         
*                                                                               
*        ZIC   R1,ITMSESTM                                                      
*        CVD   R1,DUB                                                           
*        OI    DUB+7,X'0F'                                                      
*        UNPK  QEST,DUB                                                         
*        MVC   0(3,R3),QEST                                                     
*        MVI   3(R3),X'5E'                                                      
*        LA    R3,4(R3)                                                         
*                                                                               
STHMHD80 OC    ITMSINSQ,ITMSINSQ   ANY INVOICE NUMBER SEQ #?                    
         BZ    STHMHD85            NO, WE'LL MAKE OUR OWN                       
*                                                                               
         L     R1,AINVNTBL                                                      
         USING INVNDSCT,R1                                                      
         ZICM  RF,ITMSINSQ,2                                                    
         BCTR  RF,0                                                             
         MHI   RF,L'INVNNTRY                                                    
         AR    R1,RF                                                            
         MVC   0(L'INVNNUMB,R3),INVNNUMB                                        
         B     STHMHD90                                                         
         DROP  R1                                                               
*                                                                               
STHMHD85 MVC   0(L'TODAYDTE,R3),TODAYDTE                                        
         L     R1,OURINVN                                                       
         LA    R1,1(,R1)                                                        
         ST    R1,OURINVN                                                       
         CHI   R1,9999                                                          
         BNH   *+6                                                              
         DC    H'0'                DIE IF MORE THAN 9999                        
         CVD   R1,DUB                                                           
         UNPK  6(4,R3),DUB(8)                                                   
         OI    9(R3),X'F0'                                                      
*                                                                               
STHMHD90 MVI   10(R3),X'5E'                                                     
         LA    R3,11(R3)                                                        
*                                                                               
         GOTO1 DATCON,DMCB,(2,ITMSDATE),(0,(R3))                                
         GOTO1 AGTBROAD,(R1),(1,(R3)),WORK,GETDAY,ADDAY                         
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                SHOULD BE GOOD                               
         MVC   0(4,R3),WORK+6      BRDCAST END DATE SHOULD HAVE MONTH           
         SPACE                                                                  
         CLI   0(R3),X'FA'         THIS FUNNY YEAR                              
         BL    *+18                                                             
         SR    RE,RE                                                            
         IC    RE,0(R3)                                                         
         AHI   RE,-10                                                           
         STC   RE,0(R3)                                                         
         SPACE                                                                  
         MVI   4(R3),X'5E'                                                      
         LA    R3,5(R3)                                                         
*                                                                               
         GOTO1 DATCON,(R1),(2,ITMSDATE),(X'20',(R3))                            
         MVI   6(R3),X'5E'                                                      
         LA    R3,7(R3)                                                         
* ADVANCE TO AGENCY ADVERTISER CODE FIELD                                       
         MVC   0(13,R3),=13X'5E'                                                
         LA    R3,13(R3)                                                        
*                                                                               
* STEVE K. ASKED NOT TO SEND CLT/PRD/EST CODES JAN26/05                         
*        MVC   0(L'QCLT,R3),QCLT   AGENCY CLIENT CODE                           
*        LA    R3,3(R3)                                                         
*                                                                               
         MVC   0(2,R3),=2X'5E'                                                  
         LA    R3,2(R3)                                                         
*                                                                               
* STEVE K. ASKED NOT TO SEND CLT/PRD/EST CODES JAN26/05                         
*        MVC   0(L'QPRD,R3),QPRD   AGENCY PRODUCT CODE                          
*        LA    R3,3(R3)                                                         
*                                                                               
         MVI   0(R3),X'15'                                                      
         LA    R3,1(R3)                                                         
*                                                                               
         OC    QNET,QNET           THIS A CABLE HEAD                            
         BZ    STHMHD96                                                         
         CLC   QNET,SPACES         THIS A CABLE HEAD                            
         BE    STHMHD96                                                         
*                                                                               
         BCTR  R3,0                                                             
         MVC   0(4,R3),=4X'5E'                                                  
         LA    R3,4(,R3)                                                        
*                                                                               
         MVC   0(3,R3),QNET                                                     
         MVI   3(R3),X'15'                                                      
         LA    R3,4(,R3)                                                        
*                                                                               
STHMHD96 LA    R1,OUTAREA                                                       
         SR    R3,R1                                                            
         SPACE                                                                  
         CH    R3,MAXRECSZ         MAX REC LENGTH FOR NOW                       
         BNH   *+8                                                              
         BAS   RE,MAXRECLN         GO PRINT OUT ERROR                           
         SPACE                                                                  
         STCM  R3,3,OUTRECLN                                                    
         LA    R3,OUTAREA                                                       
         L     R0,AOUTFILE                                                      
         PUT   (R0),(R3)                                                        
*                                                                               
******                                                                          
* BROADCAST DETAIL RECORD                                                       
******                                                                          
STHMBRD  XC    ELEM,ELEM                                                        
         MVC   ELEM(L'ITMSNTRY),ITMSNTRY                                        
*                                                                               
         CLI   REQTRCE,C'Y'                                                     
         BNE   STHMBD10                                                         
*                                                                               
         MVC   P(17),=CL17'BROADCAST DETAIL '                                   
         GOTO1 HEXOUT,DMCB,ELEM,P+17,L'ITMSNTRY                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
STHMBD10 DS   0H                                                                
         XC    OUTRECLN,OUTRECLN                                                
         LA    R3,OUTDATA                                                       
         MVC   0(2,R3),=C'51'      BROADCAST DETAIL RECORD CODE                 
         MVI   2(R3),X'5E'         SEMICOLONS USED TO SEPARATE FIELDS           
         LA    R3,3(R3)                                                         
         MVI   0(R3),C'Y'          ASSUME IT RAN                                
         MVI   1(R3),X'5E'         SEMICOLONS USED TO SEPARATE FIELDS           
         LA    R3,2(R3)                                                         
         GOTO1 DATCON,DMCB,(2,ITMSDATE),(X'20',0(R3))                           
         MVI   6(R3),X'5E'                                                      
         GOTO1 GETDAY,(R1),(R3),7(R3)                                           
         CLC   7(3,R3),SPACES                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   7(1,R3),DMCB        SET DAY OF WEEK                              
         OI    7(R3),X'F0'                                                      
         MVI   8(R3),X'5E'                                                      
         LA    R3,9(R3)                                                         
*                                                                               
         EDIT  (B2,ITMSTIME),(4,0(R3)),FLOAT=0                                  
         MVI   4(R3),X'5E'                                                      
         LA    R3,5(R3)                                                         
*                                                                               
         EDIT  (B1,ITMSSPLN),(3,0(R3)),FLOAT=0                                  
         MVI   3(R3),X'5E'                                                      
         LA    R3,4(R3)                                                         
*                                                                               
         OC    ITMSFILM,ITMSFILM   NO FILM CODE                                 
         BNZ   *+16                                                             
         MVI   0(R3),X'5E'                                                      
         LA    R3,1(R3)                                                         
         B     STHMBD20                                                         
*                                                                               
         L     R1,AFILMTBL         COPY THE FILM CODE                           
         USING FILMDSCT,R1                                                      
         ZIC   RF,ITMSFILM                                                      
         BCTR  RF,0                                                             
         MHI   RF,L'FILMNTRY                                                    
         AR    R1,RF                                                            
         MVC   0(L'FILMCODE,R3),FILMCODE                                        
         MVI   8(R3),X'5E'                                                      
         LA    R3,9(,R3)                                                        
*                                                                               
STHMBD20 ICM   R1,15,ITMSCOST                                                   
         CVD   R1,DUB                                                           
         ZAP   P6,DUB                                                           
         EDIT  (P6,P6),(10,0(R3)),ALIGN=LEFT,FLOAT=-,ZERO=NOBLANK               
         CLI   0(R3),C' '                                                       
         BE    *+12                                                             
         LA    R3,1(R3)                                                         
         B     *-12                                                             
*                                                                               
         AP    INVTOTAL,P6         ADD THIS AMOUNT TO THE INVOICE TOTAL         
*                                                                               
         MVI   0(R3),X'15'                                                      
         LA    R3,1(R3)                                                         
*                                                                               
         LA    R1,OUTAREA                                                       
         SR    R3,R1                                                            
         SPACE                                                                  
         CH    R3,MAXRECSZ         MAX REC LENGTH FOR NOW                       
         BNH   *+8                                                              
         BAS   RE,MAXRECLN         GO PRINT OUT ERROR                           
         SPACE                                                                  
         STCM  R3,3,OUTRECLN                                                    
         LA    R3,OUTAREA                                                       
         L     R0,AOUTFILE                                                      
         PUT   (R0),(R3)                                                        
*                                                                               
         LA    R6,1(,R6)           ADD TO ITEMS THIS INVOICE COUNT              
*                                                                               
         LA    R4,ITMSNEXT                                                      
         BCT   R5,STHMLOOP                                                      
*                                                                               
         BAS   RE,PUTINVTO                                                      
*                                                                               
SENDX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE PUTS OUT THE INVOICE TOTAL RECORD                                
***********************************************************************         
PUTINVTO NTR1                                                                   
         AP    GRSTOTAL,INVTOTAL                                                
         XC    OUTRECLN,OUTRECLN                                                
         LA    R3,OUTDATA                                                       
         MVC   0(2,R3),=C'34'      INVOICE TOTAL CODE                           
         MVI   2(R3),X'5E'                                                      
         LA    R3,3(R3)                                                         
         MVI   0(R3),X'5E'         NO INVOICE CONFIRMED COST                    
         LA    R3,1(R3)                                                         
         EDIT  (P11,INVTOTAL),(11,0(R3)),ALIGN=LEFT,FLOAT=-,           X        
               ZERO=NOBLANK                                                     
         CLI   0(R3),C' '                                                       
         BE    *+12                                                             
         LA    R3,1(R3)                                                         
         B     *-12                                                             
         MVI   0(R3),X'5E'                                                      
         MVI   1(R3),X'5E'         NO AGENCY COMMISSION                         
*                                                                               
         LA    R3,2(R3)            SAME AMOUNT FOR NET DUE                      
         EDIT  (P11,INVTOTAL),(11,0(R3)),ALIGN=LEFT,FLOAT=-,           X        
               ZERO=NOBLANK                                                     
         CLI   0(R3),C' '                                                       
         BE    *+12                                                             
         LA    R3,1(R3)                                                         
         B     *-12                                                             
*                                                                               
         MVI   0(R3),X'15'                                                      
         LA    R3,1(R3)                                                         
*                                                                               
         LA    R1,OUTAREA                                                       
         SR    R3,R1                                                            
         SPACE                                                                  
         CH    R3,MAXRECSZ         MAX REC LENGTH FOR NOW                       
         BNH   *+8                                                              
         BAS   RE,MAXRECLN         GO PRINT OUT ERROR                           
         SPACE                                                                  
         STCM  R3,3,OUTRECLN                                                    
         LA    R3,OUTAREA                                                       
         L     R0,AOUTFILE                                                      
         PUT   (R0),(R3)                                                        
*                                                                               
         MVC   PAGYNM(L'AGYNM),AGYNM      AGENCY                                
         MVC   PQMED(L'QMED),QMED         MEDIA                                 
         MVC   PSTA(L'QSTA-1),QSTA        STATION                               
         SPACE                                                                  
         CLC   QNET,SPACES                                                      
         BNH   PINV06                                                           
         MVI   PSTA+4,C'/'                                                      
         MVC   PSTA+5(3),QNET                                                   
         B     PINV10                                                           
*                                                                               
PINV06   CLI   QMED,C'T'           IF TELEVISION                                
         BE    PINV10                                                           
         CLI   QMED,C'N'             OR NETWORK                                 
         BE    PINV10              THEN NO RADIO BANDS                          
*                                                                               
         MVI   PSTA+4,C'-'          RADIO BAND                                  
         MVC   PSTA+5(1),QSTA+4                                                 
*                                                                               
PINV10   MVC   PQCLT(L'QCLT),QCLT      CLIENT CODE                              
*                                                                               
         MVC   PQPROD(3),QPRD      PRODUCT CODE                                 
*                                                                               
         MVC   PEST,QEST                                                        
*                                                                               
         L     R4,AITMSTBL         START OF BROADCAST MONTH                     
         USING ITMSDSCT,R4                                                      
         GOTO1 DATCON,DMCB,(2,ITMSDATE),(8,PBRDMO)                              
         DROP  R4                                                               
         L     R0,TOTNUMSP         TOTAL SPOTS                                  
         AR    R0,R6                                                            
         ST    R0,TOTNUMSP                                                      
*                                  # OF ITEMS AND TOTAL DOLLARS FOR INV         
         EDIT  ((R6)),(8,PITEMS),ZERO=NOBLANK                                   
         EDIT  (P11,INVTOTAL),(17,PAMOUNT),2,FLOAT=-,ZERO=NOBLANK,     X        
               COMMAS=YES                                                       
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PINVX    XIT1                                                                   
         DS    0H                                                               
***********************************************************************         
* ERROR MESSAGES                                                                
***********************************************************************         
         SPACE                                                                  
MAXRECLN NTR1                                                                   
         MVC   P+2(24),=C'RECORD SIZE EXCEEDED 360'                             
         CVD   R3,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+27(4),DUB                                                      
         MVC   P+32(30),OUTDATA                                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XIT1                                                                   
MAXRECSZ DC    H'364'                                                           
         LTORG                                                                  
         DROP  RB,RC                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE GETS THE AGENCY NAME AND ADDRESS OF THE PRINCIPAL ID             
* FOR AN AGENCY POWER CODE.                                                     
*                                                                               
* ON ENTRY:    HALF                CONTAINS THE AGENCY POWER CODE               
***********************************************************************         
         SPACE                                                                  
GETAGYNM NMOD1 0,*GETAGY*                                                       
         L     RC,SVRC                                                          
         USING GEND,RC                                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING CT5KEY,R3                                                        
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,HALF                                                    
         DROP  R3                                                               
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',(R3),(R6)                 
*                                                                               
         USING CT5REC,R6                                                        
         CLC   CT5KEY,KEY                                                       
         BNE   INVLFLDG            INVALID AGENCY POWER CODE                    
         LA    R6,CT5DATA                                                       
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GFIRSTEL                                                      
         BE    *+6                 DIE IF NO PRINCIPAL ID NUM                   
         DC    H'0'                                                             
*                                                                               
         XC    KEY,KEY                                                          
         USING CTIREC,R3                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,2(R6)       GET THE PRINCIPAL ID NUM                     
         DROP  R3                                                               
         L     R6,AIO                                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',(R3),(R6)                 
*                                                                               
         USING CTIREC,R6                                                        
         CLC   CTIKEY,KEY                                                       
         BE    *+6                 DIE IF INVALID PRINCIPAL ID NUM              
         DC    H'0'                                                             
         LA    R6,CTIDATA                                                       
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'36'        GET ID NAME AND ADDRESS                      
         BAS   RE,GFIRSTEL                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CTORGD,R6                                                        
         MVC   AGYNM,CTORGNAM                                                   
         MVC   AGYADRES,CTORGADD                                                
         DROP  R6                                                               
*                                                                               
GAGYX    MVC   AIO,AIO1                                                         
         XIT1                                                                   
         SPACE                                                                  
INVLFLDG MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
GFIRSTEL CLI   0(R6),0                                                          
         BNE   *+10                                                             
         CLI   0(R6),1                                                          
         BR    RE                                                               
         CLI   ELCODE,0                                                         
         BCR   8,RE                                                             
         CLC   ELCODE,0(R6)                                                     
         BCR   8,RE                                                             
GNEXTEL  SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         LTR   RF,RF                                                            
         BNZ   *+10                                                             
         CLI   1(R6),1                                                          
         BR    RE                                                               
         AR    R6,RF                                                            
         B     GFIRSTEL                                                         
         LTORG                                                                  
         DROP  RB,RC                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE GETS THE ID RECORD BASED ON THE AGENCY POWER CODE THAT           
* IS STORED IN THE ESTIMATE FILTER.                                             
*                                                                               
* ON ENTRY:    (R4)                A(ITEM ENTRY)                                
*              (R6)                A(ITEM ELEMENT)                              
***********************************************************************         
         SPACE                                                                  
GETIDREC NMOD1 0,*GETID**                                                       
         L     RC,SVRC                                                          
         USING GEND,RC                                                          
         LR    R2,R6               USE R2 INSTEAD OF R6                         
         USING ITMSDSCT,R4                                                      
         USING INVELEM,R2                                                       
         MVC   AIO,AIO2                                                         
*                                                                               
         MVI   ESTFLAG,C'N'        DON'T ADD ENTRY JUST YET                     
*                                                                               
         CLI   BEST,0              IF EST IS ZERO                               
         BNE   GID00                NO, DO ESTIMATE CHECK                       
*                                                                               
         CLC   AGENCY,=C'AP'       ALLOW APPLE TO BYPASS ESTIMATE CK            
         BE    GID01                YES, BYPASS ESTIMATE CHECK                  
*                                                                               
         CLC   AGENCY,=C'LI'       ALLOW LIPA TO BYPASS ESTIMATE CK             
         BE    GID01                YES, BYPASS ESTIMATE CHECK                  
*                                                                               
*                                                                               
GID00    CLI   REQAGY+2,C'*'       THIS A DDS GEN REQUEST                       
         BNE   GID02                YES, USE THAT POWER CODE                    
*                                                                               
GID01    MVC   ITMSAGPC,REQAGY     SAVE AGENCY POWER CODE                       
         B     GID50                YES, USE THAT POWER CODE                    
*                                                                               
GID02    CLI   NUMBESTM,0          ANY ENTRIES IN OUR ESTIMATE TABLE?           
         BE    GID10               NONE                                         
*                                                                               
         ZIC   R0,NUMBESTM         MAKE SURE WE DON'T GO BEYOND TABLE           
         L     R1,AESTMTBL                                                      
         USING ESTMDSCT,R1                                                      
GID03    CLC   INVPRD(2),ESTMPROD  SEE IF THIS PRD & EST IN TABLE               
         BNE   GID06                                                            
         OC    ESTMPOWC,ESTMPOWC   IF NO POWER CODE THEN                        
         BZ    MISSPOWC            MISSING POWER CODE                           
         BO    INVLPOWC            X'FFFF' - INVALID POWER CODE                 
         OC    ESTMPOWC(1),ESTMPOWC  X'FF' - INVALID ESTIMATE CODE              
         BO    INVLESTM                                                         
         MVC   ITMSAGPC,ESTMPOWC   GOT THE POWER CODE BEFORE                    
         B     GIDYES                                                           
*                                                                               
GID06    LA    R1,ESTMNEXT                                                      
         BCT   R0,GID03                                                         
         DROP  R1                                                               
*                                                                               
GID10    MVI   ESTFLAG,C'Y'        ADD ENTRY LATER                              
*                                                                               
         XC    KEY,KEY             SET UP THE ESTIMATE KEY                      
         LA    R3,KEY                                                           
         USING EKEY,R3                                                          
*                                                                               
         MVC   EKEYAM,LSTAGYMD     USE THE AGENCY/MEDIA                         
         MVC   EKEYCLT,BCLT            AND CLIENT FROM INVOICE RECORD           
*                                                                               
         MVC   EKEYEST,ITMSESTM                                                 
*                                                                               
         LA    RE,SVCLIST          GO THROUGH CLIENT PRODUCT LIST               
         LA    R0,255                                                           
GID20    OC    0(4,RE),0(RE)                                                    
         BZ    INVLPROD            INVALID PRODUCT CODE IN ITEM                 
         CLC   ITMSPROD,3(RE)                                                   
         BE    GID30                                                            
         LA    RE,4(RE)                                                         
         BCT   R0,GID20                                                         
*                                                                               
GID30    MVC   EKEYPRD,0(RE)                                                    
         DROP  R3                                                               
*                                                                               
         MVC   FILENAME,=CL8'SPTDIR'                                            
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
*                                                                               
         CLC   KEY(L'EKEY),KEYSAVE                                              
         BNE   INVLESTM            INVALID ESTIMATE                             
*                                                                               
         MVC   FILENAME,=CL8'SPTFIL'                                            
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
*                                                                               
         L     R6,AIO                                                           
         USING ESTHDRD,R6                                                       
         OC    EPROF(2),EPROF      ANY AGENCY POWER CODE HERE?                  
         BZ    MISSPOWC            NONE, WE WANT ONE                            
*                                                                               
         TM    BITFLAG1,X'08'      FILTER ON AGENCY POWER CODE?                 
         BZ    *+14                                                             
         CLC   REQAGY,EPROF                                                     
         BNE   GIDNO                                                            
*                                                                               
         MVC   ITMSAGPC,EPROF      SAVE AGENCY POWER CODE                       
*                                                                               
GID50    XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING CT5KEY,R3                                                        
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,ITMSAGPC                                                
         DROP  R3                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,AIO                   
*                                                                               
         L     R6,AIO                                                           
         USING CT5REC,R6                                                        
         CLC   CT5KEY,KEY                                                       
         BNE   INVLPOWC            INVALID AGENCY POWER CODE                    
         LA    R6,CT5DATA                                                       
*                                                                               
         MVI   ELCODE,X'02'                                                     
         BAS   RE,IFIRSTEL                                                      
         BE    *+6                                                              
         DC    H'0'                DIE IF NO PRINCIPAL ID NUM                   
*                                                                               
         CLI   NUMBESTM,MAXESTMS   ANY MORE ROOM IN ESTIMATE TABLE?             
         BE    GIDYES              NO                                           
*                                                                               
         L     R1,AESTMTBL                                                      
         ZIC   R0,NUMBESTM                                                      
         MHI   R0,L'ESTMNTRY                                                    
         AR    R1,R0                                                            
         USING ESTMNTRY,R1                                                      
         MVC   ESTMPROD(2),ITMSPROD  FOR THIS PRODUCT & ESTIMATE                
         MVC   ESTMPOWC,ITMSAGPC   SAVE CODE FOR ERROR                          
         DROP  R1                                                               
*                                                                               
         ZIC   R1,NUMBESTM         INCREMENT # OF ESTIMATE ENTRIES              
         LA    R1,1(R1)                                                         
         STC   R1,NUMBESTM                                                      
         B     GIDYES                                                           
*                                                                               
INVLPROD BAS   RE,BADGITEM                                                      
         MVC   P2+4(25),=CL25'HAS A BAD PRODUCT CODE OF '                       
         GOTO1 HEXOUT,DMCB,INVPRD,P2+30,L'INVPRD                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     GIDNO                                                            
*                                                                               
INVLESTM BAS   RE,BADGITEM                                                      
         MVC   P2+4(26),=CL26'HAS A BAD ESTIMATE CODE OF '                      
         GOTO1 HEXOUT,DMCB,INVPRD2,P2+31,L'INVPRD2                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   HALF,=X'FFFE'       CODE FOR INVALID ESTIMATE IN TABLE           
         B     GIDERR                                                           
*                                                                               
MISSPOWC LR    R6,R2                                                            
         BAS   RE,BADGITEM                                                      
         MVC   P2+4(19),=CL19'HAS AN ESTIMATE OF '                              
         GOTO1 HEXOUT,DMCB,INVPRD2,P2+24,L'INVPRD2                              
         MVC   P2+27(18),=CL18'WITH NO POWER CODE'                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XC    HALF,HALF           CODE FOR MISSING POWER CD IN TABLE           
         B     GIDERR                                                           
*                                                                               
INVLPOWC LR    R6,R2                                                            
         BAS   RE,BADGITEM                                                      
         MVC   P2+4(19),=CL19'HAS AN ESTIMATE OF '                              
         GOTO1 HEXOUT,DMCB,INVPRD2,P2+24,L'INVPRD2                              
         MVC   P2+27(25),=CL25'WITH A BAD POWER CODE OF '                       
         MVC   P2+53(L'ITMSAGPC),ITMSAGPC                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   HALF,=X'FFFF'       CODE FOR INVALID POWER CD IN TABLE           
         B     GIDERR                                                           
*                                                                               
GIDERR   CLI   ESTFLAG,C'Y'        ADD AN ESTIMATE ENTRY?                       
         BNE   GIDNO               NO                                           
*                                                                               
         CLI   NUMBESTM,MAXESTMS   ANY MORE ROOM IN ESTIMATE TABLE?             
         BE    GIDNO               NO                                           
*                                                                               
         L     R1,AESTMTBL                                                      
         ZIC   R0,NUMBESTM                                                      
         MHI   R0,L'ESTMNTRY                                                    
         AR    R1,R0                                                            
         USING ESTMNTRY,R1                                                      
         MVC   ESTMPROD(2),ITMSPROD  FOR THIS PRODUCT & ESTIMATE                
         MVC   ESTMPOWC,HALF       SAVE CODE FOR ERROR                          
         DROP  R1                                                               
*                                                                               
         ZIC   R1,NUMBESTM         INCREMENT # OF ESTIMATE ENTRIES              
         LA    R1,1(R1)                                                         
         STC   R1,NUMBESTM                                                      
*                                                                               
GIDNO    MVC   AIO,AIO1                                                         
         LTR   RB,RB                                                            
         XIT1                                                                   
*                                                                               
GIDYES   MVC   AIO,AIO1                                                         
         CR    RC,RC                                                            
         XIT1                                                                   
*                                                                               
BADGITEM MVC   P(39),=CL39'THIS RECORD HAS AN INVOICE ITEM DATED: '             
         LR    R0,RE                                                            
         GOTO1 DATCON,DMCB,(2,INVDAT),(11,P+39)                                 
         LR    RE,R0                                                            
         MVC   P+48(11),=CL11'AND TIMED: '                                      
         EDIT  (B2,INVTIM),(4,P+59),FILL=0                                      
         BR    RE                                                               
         DROP  R2,R6                                                            
*                                                                               
IFIRSTEL CLI   0(R6),0                                                          
         BNE   *+10                                                             
         CLI   0(R6),1                                                          
         BR    RE                                                               
         CLI   ELCODE,0                                                         
         BCR   8,RE                                                             
         CLC   ELCODE,0(R6)                                                     
         BCR   8,RE                                                             
INEXTEL  SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         LTR   RF,RF                                                            
         BNZ   *+10                                                             
         CLI   1(R6),1                                                          
         BR    RE                                                               
         AR    R6,RF                                                            
         B     IFIRSTEL                                                         
         LTORG                                                                  
         DROP  RB,RC                                                            
         EJECT                                                                  
       ++INCLUDE SPEZFFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPEZFB1D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPEZFWORKD                                                     
         EJECT                                                                  
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
INVRECD  DSECT                                                                  
       ++INCLUDE SPGENINV                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSNV                                                       
*                                                                               
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDPERVALD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDPERVALD                                                      
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
         PRINT ON                                                               
         EJECT                                                                  
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
RELO     DS    A                                                                
AGTBROAD DS    V                   V(GETBROAD)                                  
AESTMTBL DS    A                   A(OUR SAVED FILM TABLE)                      
ASFLMTBL DS    A                   A(OUR SAVED FILM TABLE)                      
AFILMTBL DS    A                   A(OUR FILM TABLE)                            
AINVNTBL DS    A                   A(OUR INVOICE NUMBER TABLE)                  
ANXTINVN DS    A                   A(NEXT INVOICE ITEM ENTRY)                   
AITMSTBL DS    A                   A(OUR INVOICE ITEM TABLE)                    
ANXTITMS DS    A                   A(NEXT INVOICE ITEM ENTRY)                   
AOUTFILE DS    A                   A(OUTFILE DCB - STORED IN TSPFUSER)          
QSORT    DS    A                   A(QSORT)                                     
SVRC     DS    A                   SAVED REGISTER C                             
         SPACE                                                                  
EZPROF   DS   0CL16                                                             
EZPROF01 DS    CL1                                                              
EZPROF02 DS    CL1                                                              
EZPROF03 DS    CL1                                                              
EZPROF04 DS    CL1                                                              
EZPROF05 DS    CL1                                                              
EZPROF06 DS    CL1                                                              
EZPROF07 DS    CL1                 INV GEN-FORCE ZERO COST                      
EZPROF08 DS    CL1                                                              
EZPROF09 DS    CL1                                                              
EZPROF10 DS    CL1                                                              
EZPROF11 DS    CL1                                                              
EZPROF12 DS    CL1                                                              
EZPROF13 DS    CL1                                                              
EZPROF14 DS    CL1                                                              
EZPROF15 DS    CL1                                                              
EZPROF16 DS    CL1                                                              
*                                                                               
NUMBITMS DS    F                   NUMBER OF INVOICE ITEMS IN TABLE             
OURINVN  DS    F                   # FOR OUR INVOICE NUMBERS (1-9999)           
TOTNUMIN DS    F                   TOTAL NUMBER OF INVOICES                     
TOTNUMSP DS    F                   TOTAL NUMBER OF SPOTS                        
SVESTAGY DS    CL2                 AGENCY POWER CODE FROM ESTIMATE              
NUMBESTM DS    XL1                 NUMBER OF ESTIMATE ENTRIES                   
NUMBFILM DS    XL1                 NUMBER OF FILM ENTRIES                       
NUMBINVN DS    XL2                 NUMBER OF INVOICE NUMBERS                    
*                                                                               
SVSNVPRD DS    XL1                                                              
SVSNVPR2 DS    XL1                                                              
SVSNVEST DS    XL1                                                              
SVSNVSDT DS    XL2                                                              
*                                                                               
LSTAGYMD DS    XL1                 LAST AGENCY/MEDIA CODE                       
LSTCLNT  DS    XL2                 LAST CLIENT                                  
LSTMRKT  DS    XL2                 ONLY USED FOR MSUNPK                         
LSTSTATN DS    XL3                 LAST STATION                                 
LSTINVNO DS    CL10                LAST INVOICE                                 
*                                                                               
LSTBDMTH DS    0CL12               LAST BROADCAST MONTH (YYMMDD-YYMMDD)         
LSTBDDTS DS    CL6                 LAST BROADCAST MONTH START DATE              
LSTBDDTN DS    CL6                 LAST BROADCAST MONTH END DATE                
*                                                                               
SNTPOWER DS    CL2                 LAST AGENCY POWER CODE SENT                  
SNTAGYMD DS    XL1                 LAST AGENCY/MEDIA SENT                       
SNTSTATN DS    XL3                 LAST STATION SENT                            
SNTPRDCD DS    XL1                 LAST PRODUCT CODE SENT                       
SNTESTCD DS    XL1                 LAST ESTIMATE CODE SENT                      
SNTINOSQ DS    XL2                 LAST SEQ # TO OUR INVOICE # TABLE            
*                                                                               
AGYNM    DS    CL25                AGENCY'S NAME                                
AGYADRES DS    CL30                AGENCY'S ADDRESS                             
*                                                                               
SGNONAGY DS    CL25                SIGN-ON AGENCY'S NAME                        
*                                                                               
FILTFLAG DS    XL1                 FILTERS BEING USED                           
FILTMEDQ EQU   X'80'               FILTER ON MEDIA                              
FILTCLTQ EQU   X'40'                         CLIENT                             
FILTPRDQ EQU   X'20'                         PRODUCT                            
FILTESTQ EQU   X'10'                         ESTIMATE                           
FILTBRDQ EQU   X'08'                         BROADCAST MONTH                    
FILTAGYQ EQU   X'04'                         AGENCY                             
FILTSTAQ EQU   X'02'                         STATION                            
FILTRRNQ EQU   X'01'                         RERUN DATE                         
*                                                                               
BITFLAG1 DS    XL1                 VARIOUS FLAGS                                
*                                  X'01' = PUT OUT INVOICE TOTAL RECORD         
*                                                                               
BITFLAG2 DS    XL1                 X'80' = ITEM WAS USED FROM RECORD            
BTITMUSD EQU   X'80'                                                            
*                    FOLLOWING ONLY USED FOR NEW INVOICE RECS                   
*                                                                               
BTITMULT EQU   X'40'               X'40' HAD A MULTI RECORD SET                 
*                                                                               
ESTFLAG  DS    XL1                 C'Y' = ADD ENTRY TO ESTIMATE TABLE           
TODAYDTE DS    CL6                 TODAY'S DATE (YYMMDD)                        
BROADDTE DS    CL6                 BROADCAST DATE (YYMMDD)                      
RRUNDATE DS    XL3                 RERUN DATE (BINARY)                          
*                                                                               
ADATE    DS    CL6                 USED JUST FOR A DATE                         
*                                                                               
FILTYRMO DS    XL2                 BROADCAST MONTH WITH DAY 02 NEW INV          
*                                                                               
FILTBROD DS    0CL12               FILTER BROADCAST AREA                        
FILTBRST DS    CL6                 FILTER BROADCAST START DATE                  
FILTBRND DS    CL6                 FILTER BROADCAST END DATE                    
*                                                                               
FILTNAME DS    CL33                FILTER AGENCY NAME                           
FILTADDR DS    CL33                FILTER AGENCY ADDRESS                        
*                                                                               
KEYBROAD DS    0CL12               KEY'S BROADCAST DATE AREA                    
KEYBRDST DS    CL6                 KEY'S BROADCAST START DATE                   
KEYBRDND DS    CL6                 KEY'S BROADCAST END DATE                     
*                                                                               
BRDAREA  DS    0CL12               BROADCAST DATE AREA                          
BRDSTART DS    CL6                 BROADCAST START DATE                         
BRDEND   DS    CL6                 BROADCAST END DATE                           
*                                                                               
FAKEFHDR DS    CL8                 FAKE FIELD HEADER                            
FAKEFDAT DS    CL8                 FAKE FIELD DATA                              
*                                                                               
P6       DS    PL6                 PACKED FIELD                                 
*                                                                               
INVTOTAL DS    PL11                INVOICE TOTAL                                
GRSTOTAL DS    PL11                GROSS TOTAL                                  
*                                                                               
LSTINVKY DS    CL32                LAST INVOICE KEY USED FOR SEQ INV            
*                                                                               
PERVALST DS    XL56                PERVAL STORAGE AREA                          
*                                                                               
*        OUTPUT RECORD AREA FOR PUT MACRO                                       
*                                                                               
OUTAREA  DS    0XL(L'OUTRECLN+L'OUTDATA)                                        
OUTRECLN DS    XL4                 LENGTH                                       
OUTDATA  DS    XL768               RECORD DATA                                  
*                                                                               
WRKFEND  EQU   *                                                                
*                                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPEZF21TBL                                                     
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         EJECT                                                                  
* OFFLINE REPORT                                                                
         SPACE                                                                  
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL2                                                              
PAGYNM   DS    CL25                                                             
         DS    CL3                                                              
PQMED    DS    CL1                                                              
         DS    CL5                                                              
PSTA     DS    CL6                                                              
         DS    CL5                                                              
PQCLT    DS    CL3                                                              
         DS    CL7                                                              
PQPROD   DS    CL3                                                              
         DS    CL4                                                              
PEST     DS    CL3                                                              
         DS    CL6                                                              
PBRDMO   DS    CL8                                                              
         DS    CL3                                                              
PITEMS   DS    CL8                                                              
PAMOUNT  DS    CL17                                                             
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'037SPEZF21   03/04/05'                                      
         END                                                                    
