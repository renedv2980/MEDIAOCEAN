*          DATA SET MZEIEST    AT LEVEL 186 AS OF 05/01/02                      
*PHASE MZEIEST                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T21750  -- ESTIMATE MAINTENANCE                      *         
*                                                                     *         
*  COMMENTS:     MAINTAINS ESTIMATE RECORDS                           *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T21900), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREEN SPSFM70 (MAINT)                               *         
*                                                                     *         
*  OUTPUTS:                                                           *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- RECORD                                         *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOL                                          *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21750 - ESTIMATE MAINTENANCE'                                  
T21750   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1750**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         BAS   RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,RECDEL         DELETE RECORD                                
         BE    DEL                                                              
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE KEY                                  *         
***********************************************************************         
*                                                                               
VK       LA    R3,KEY                                                           
         USING ESTHDR,R3                                                        
         XC    KEY,KEY                                                          
         MVI   EKEYTYPE,X'00'                                                   
*                                                                               
         MVC   ESTMEDN,SPACES        CLEAR MEDIA NAME AND CLIENT NAME           
         OI    ESTMEDNH+6,X'80'      AND PRODUCT NAME                           
         MVC   ESTCLIN,SPACES                                                   
         OI    ESTCLINH+6,X'80'                                                 
         MVC   ESTPRDN,SPACES                                                   
         OI    ESTPRDNH+6,X'80'                                                 
*                                                                               
***********************************************************************         
*                                                                               
         LA    R2,ESTMEDKH           MEDIA                                      
         GOTO1 VALIMED               VALIDATE MEDIA CODE AND TRANSMIT           
         MVC   ESTMEDN,MEDNM         MEDIA NAME                                 
         OI    ESTMEDNH+6,X'80'                                                 
*                                                                               
         MVC   EKEYAM,BAGYMD         MEDIA INTO KEY                             
*                                                                               
***********************************************************************         
*                                                                               
         LA    R2,ESTCLIKH           CLIENT                                     
         GOTO1 VALICLT               VALIDATE CLIENT CODE AND TRANSMIT          
         MVC   ESTCLIN,CLTNM         CLIENT NAME                                
         OI    ESTCLINH+6,X'80'                                                 
*                                                                               
         L     RE,AIO                                                           
         USING CLTHDR,RE                                                        
         MVC   SVP1USER,CPU1         SAVE CLIENT RECORD DATA                    
         MVC   SVP1TYPE,CPU1TYPE                                                
         MVC   SVP1LEN,CPU1LEN                                                  
         MVC   SVP1FLG1,CPU1FLG1                                                
         MVC   SVP1FLG2,CPU2FLG2                                                
         MVC   SVP2USER,CPU2                                                    
         MVC   SVP2TYPE,CPU2TYPE                                                
         MVC   SVP2LEN,CPU2LEN                                                  
         MVC   SVP2FLG1,CPU2FLG1                                                
         MVC   SVP2FLG2,CPU2FLG2                                                
         MVC   SVCLOP1,COPT1                                                    
         MVC   SVCLOP2,COPT2                                                    
         MVC   SVCLTPOL,CPOLONLY                                                
         MVC   SVCLPROF,CPROF                                                   
         MVC   SVCLEX,CEXTRA                                                    
         MVC   SVCLDLY,CDAILY                                                   
         MVC   SVCLTPW,CPWPCT                                                   
         MVC   SVCCOST2,CCOST2                                                  
         DROP  RE                                                               
*                                                                               
         MVC   EKEYCLT,BCLT          CLIENT INTO KEY                            
*                                                                               
***********************************************************************         
*                                                                               
         LA    R2,ESTPROKH           PRODUCT                                    
         MVI   AAAOK,C'Y'                                                       
         GOTO1 VALIPRD                                                          
         MVI   AAAOK,C'N'                                                       
         MVC   ESTPRON,PRDNM         VALIDATE PRODUCT CODE AND TRANSMIT         
         OI    ESTPRONH+6,X'80'      PRODUCT NAME                               
*                                                                               
         L     RE,AIO                SAVE PRODUCT REC DATA                      
         USING PRDHDR,RE                                                        
*                                                                               
         DROP  RE                                                               
*                                                                               
         CLC   SVCTAGY,=C'CK'        FOR AGENCY CK...                           
         BNE   VK10                                                             
         CLC   QCLT,=C'CC '                                                     
         BE    VK10                                                             
         MVC   ERRNUM,=AL2(VKERR4)   NO ADD OF PRODUCT TO CLIENTS               
         CLI   ACTEQU,ACTADD         OTHER THAN CC                              
         BE    SPERREX                                                          
*                                                                               
VK10     MVC   EKEYPRD,QPRD          PRODUCT INTO KEY                           
         OC    EKEYPRD,SPACES                                                   
*                                                                               
***********************************************************************         
*                                                                               
         LA    R2,ESTESTKH                                                      
         CLI   ACTEQU,ACTADD         ACTION ADD?                                
         BE    VK20                                                             
*                                                                               
         GOTO1 VALIEST               ACTION IS NOT ADD SO...                    
         MVC   ESTESTN,ESTNM         VALIDATE ESTIMATE CODE AND                 
         OI    ESTESTNH+6,X'80'      TRANSMIT ESTIMATE NAME                     
         B     VKX                                                              
*                                                                               
VK20     CLI   5(R2),0                ACTION IS ADD SO...                       
         BE    ERRMIS                 LENGTH MUST BE > 0                        
*                                                                               
         MVC   ERRNUM,=AL2(ERROR)                                               
         TM    ESTESTKH+4,X'08'       NUMERIC?                                  
         BZ    SPERREX                                                          
*                                                                               
         MVC   ERRNUM,=AL2(ERROR)     LENGTH MUST BE <= 3                       
         CLI   5(R2),3                                                          
         BH    SPERREX                                                          
*                                                                               
         ZIC   RE,5(R2)               CONVERT ESTIMATE CODE TO BIN              
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   RE,DUB                                                           
*                                                                               
         MVC   ERRNUM,=AL2(ERROR)     TEST IN RANGE 1 - 255                     
         CH    RE,=H'1'                                                         
         BL    SPERREX                                                          
         CH    RE,=H'255'                                                       
         BH    SPERREX                                                          
         STC   RE,BEST                SET BINARY ESTIMATE                       
*                                                                               
         MVC   EKEYEST,BEST           SAVE ESTIMATE CODE INTO KEY               
         MVC   ESTKEY,KEY             SAVE ESTIMATE RECORD KEY                  
*                                                                               
**********************************************************************          
*                                                                               
         MVC   KEY(13),ESTKEY         READ ESTIMATE RECORD FOR POL              
         MVC   KEY+4(3),=C'POL'       PRODUCT                                   
         GOTO1 HIGH                                                             
*                                                                               
         CLI   SVCLTPOL,C'Y'                                                    
         BE    VK30                                                             
         CLI   SVCLPROF+0,C'0'                                                  
         BE    VK40                                                             
VK30     CLI   BPRD,X'FF'                                                       
         BE    VK40                                                             
         MVC   ERRNUM,=AL2(ERROR)     IF CLIENT CAN ONLY BUY POL OR             
         CLC   KEY(13),KEYSAVE        BRAND POOL CLIENT TRIES TO ADD            
         BNE   SPERREX                EST FOR NON-POL PRO ... ERROR             
*                                                                               
VK40     MVC   ERRNUM,=AL2(ERROR)                                               
         CLI   BPRD,X'FF'                                                       
         BE    VK60                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    VK50                                                             
         CLI   QMED,C'N'                                                        
         BE    *+12                                                             
         CLI   SVCLTPOL,C'Y'          IF NETWORK MEDIA OR CLIENT CAN            
         BNE   VK60                   ONLY BUY POL TRIES TO ADD EST             
         B     SPERREX                FOR NON-POL PRODUCT ... ERROR             
*                                                                               
VK50     MVC   ERRNUM,=AL2(ERROR)                                               
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         CLI   EMSTRIND,0             IF POL MSTR OR SUB EST OPEN ...           
         BNE   SPERREX                ERROR                                     
*                                                                               
VK60     MVC   ERRNUM,=AL2(ERROR)                                               
         OC    SVCLTPW,SVCLTPW                                                  
         BZ    VKX                                                              
         CLC   KEY(13),KEYSAVE        POL EST MUST BE OPEN FIRST FOR            
         BNE   SPERREX                PW CLIENTS                                
*                                                                               
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         MVC   SVPOLPW,EPWPCT                                                   
         DROP  R3                                                               
*                                                                               
***********************************************************************         
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE RECORD                               *         
***********************************************************************         
VR       L     R3,AIO                ESTIMATE RECORD AT AIO                     
         USING ESTHDR,R3                                                        
         MVC   ELEN,=H'500'                                                     
*                                                                               
***********************************************************************         
*                                                                               
         CLI   ACTEQU,ACTADD       IF ACTION IS ADD ...                         
         BNE   VR20                                                             
*                                                                               
         CLI   ESTOPTH+5,0         IF OPTIONS INPUTTED, CLEAR OTPION            
         BNE   VR30                RECORD FIELD                                 
         MVC   ESTOPT,SPACES                                                    
*                                                                               
         CLI   SVCLDLY,C'Y'        IF OPTIONS SET FOR DAILY, UPDATE             
         BNE   VR10                  OPTION RECORD FIELD                        
         MVI   ESTOPTH+5,7                                                      
         MVC   ESTOPT(7),=C'DAILY=Y'                                            
VR10     OI    ESTOPTH+6,X'80'                                                  
         B     VR30                                                             
*                                                                               
***********************************************************************         
*                                                                               
VR20     CLI   SVAPROF+7,C'C'        IF ACTION IS NOT ADD, AGENCY IS            
         BNE   VR30                  CANADIAN AND MEDIA IS NETWORK OR           
         CLI   QMED,C'N'             COMBINED CHANGE AUTHORITY $'S              
         BE    EDT10                                                            
         CLI   QMED,C'C'                                                        
         BE    EDT10                                                            
*                                                                               
***********************************************************************         
*                                                                               
VR30     LA    R2,ESTSTATH           IF CHECK STATUS CHANGE IS 'S' END          
         BAS   RE,CHKSTAT                                                       
         CLI   WORK,C'S'                                                        
         BE    VRX                                                              
*                                                                               
***********************************************************************         
*                                                                               
VR40     LA    R2,ESTDESCH           DESCRIPTION REQUIRED                       
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
*                                                                               
         MVI   WORK2,C' '                                                       
         MVI   SONIA,C' '                                                       
         CLC   =C'JILLREBER',ESTDESC                                            
         BE    VR50                                                             
         CLC   =C'SONIAPRICE',ESTDESC                                           
         BE    VR50                                                             
         CLC   =C'KENSUPEK',ESTDESC  IF DESCRIPTION IS JILLREBER,               
         BNE   VR60                  SONIAPRICE OR KENSUPEK ...                 
VR50     MVI   WORK2,C'D'            ALLOW DATE CHANGES W/O VALIDATING          
         MVI   SONIA,C'C'            ALLOW CHANGE OF CASH PRD                   
         B     VR70                  AND DON'T CHANGE DESCRIPTION               
*                                                                               
*                                    OTHERWISE ... DO NOT ALLOW DATE            
VR60     MVC   EDESC,ESTDESC         CHNG W/0 VALID. DO NOT ALLOW CHNGE         
         OC    EDESC,SPACES          OF CASH PRO & CHANGE DESCRIPTION           
*                                                                               
***********************************************************************         
*                                                                               
VR70     LA    R2,ESTSTRDH           START DATE REQUIRED                        
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
*                                                                               
         MVC   ERRNUM,=AL2(BADDATE)                                             
         GOTO1 DATVAL,DMCB,(0,ESTSTRD),SYR                                      
         OC    DMCB(4),DMCB          START DATE MUST BE VALID M/D/Y             
         BZ    SPERREX                                                          
*                                                                               
         MVC   ERRNUM,=AL2(ERROR)    (CANNOT CHANGE FIELD)                      
         CLI   ACTEQU,ACTADD                                                    
         BE    VR100                                                            
         CLI   WORK2,C'D'            IF ACTION IS NOT ADD, DATES CAN BE         
         BE    VR100                 CHANGED W/O VALIDATING, MASTER OR          
         CLI   EMSTRIND,0            SUB ESTIMATE AND ESTIMATE START            
         BE    VR80                  DATE IS NOT EQUAL TO INPUTTED DATE         
         CLC   ESTART,SYR            RETURN ERROR                               
         BE    VR100                                                            
         B     SPERREX                                                          
*                                                                               
VR80     MVC   ERRNUM,=AL2(ERROR)    IF ACTION IS NOT ADD, DATES CAN BE         
         OC    ECPPEST,ECPPEST       CHANGED WITHOUT VALIDATING, NON-           
         BNZ   VR90                  MASTER OR NON-SUB ESTIMATE, CPP            
         BAS   RE,CKLINID            EST.IS ZERO AND TERMINAL IS NOT            
         BE    VR100                 AUTHORIZED ... RETURN ERROR                
VR90     CLC   ESTART,SYR                                                       
         BL    SPERREX               (CANNOT SHORTEN DURATION)                  
*                                                                               
***********************************************************************         
*                                                                               
VR100    LA    R2,ESTENDDH           END DATE REQUIRED                          
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
*                                                                               
         MVC   ERRNUM,=AL2(BADDATE)                                             
         GOTO1 DATVAL,DMCB,(0,ESTENDD),EYR                                      
         OC    DMCB(4),DMCB          END DATE MUST BE VALID M/D/Y               
         BZ    SPERREX                                                          
*                                                                               
         MVC   ERRNUM,=AL2(ERROR)                                               
         CLI   ACTEQU,ACTADD                                                    
         BE    VR130                 IF ACTION IS NOT ADD, DATES CAN            
         CLI   WORK2,C'D'            CHANGE W/0 VALIDATING, MASTER OR           
         BE    VR130                 SUB ESTIMATE AND ESTIMATE START            
         CLI   EMSTRIND,0            DATE IS NOT EQUAL TO INPUTTED              
         BE    VR110                 DATE ... RETURN ERROR                      
         CLC   EEND,EYR                                                         
         BE    VR360                                                            
         B     SPERREX                                                          
*                                                                               
VR100    MVC   ERRNUM,=AL2(ERROR)    IF ACTION IS NOT ADD, DATES CAN BE         
         OC    ECPPEST,ECPPEST       CHANGED WITHOUT VALIDATING, NON-           
         BNZ   VR120                 MASTER OR NON-SUB ESTIMATE, CPP            
         BAS   RE,CKLINID            .EST IS ZERO AND TERMINAL IS NOT           
         BE    VR130                 AUTHORIZED ... RETURN ERROR                
VR120    CLC   EEND,EYR                                                         
         BH    SPERREX               (CANNOT SHORTEN DURATION)                  
*                                                                               
VR130    MVC   ERRNUM,=AL2(ERROR)    END DATE MUST BE LATER THAN START          
         CLC   SYR(6),EYR            DATE                                       
         BH    SPERREX                                                          
*                                                                               
***********************************************************************         
*                                                                               
VR140    CLI   OVSYS,3                                                          
         BNE   VR170                                                            
         MVC   ERRNUM,=AL2(ERROR)    IF SYSTEM IS NETPAK AND MEDIA IS           
         LA    R2,ESTMEDKH           NOT N, RETURN ERROR                        
         CLI   QMED,C'N'                                                        
         BNE   SPERREX                                                          
*                                                                               
         CLI   ACTEQU,ACTADD         STATUS REQUIRED FOR NETPAK ADDS            
         BNE   VR180                                                            
         LA    R2,ESTSTATH                                                      
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         B     VR180                                                            
*                                                                               
VR170    LA    R2,ESTSTATH                                                      
         CLC   ESTSTAT(3),=C'OLD'    'OLD' AND 'NEW' INVALID STATUS             
         BE    ERRINV                FOR NON-NETPAK ESTIMATES                   
         CLC   ESTSTAT(3),=C'NEW'                                               
         BE    ERRINV                                                           
*                                                                               
VR180    CLI   ACTEQU,ACTADD         IF ACTION NOT ADD OR START                 
         BE    VR190                 DATE OR END DATE HAVE CHANGED              
         CLC   ESTART,SYR            CHECK BROADCAST MONTHS AND WKS             
         BNE   VR190                                                            
         CLC   EEND,EYR                                                         
         BE    VR220                                                            
VR190    BAS   RE,CHKEDTS                                                       
*                                                                               
VR210    LA    R2,ESTENDDH                                                      
         MVC   ERRNUM,=AL2(ERROR)    (CAN'T HAVE PW ON EST>14 WEEKS)            
         OC    EPWPCT,EPWPCT         IF TEST PW PCT PRESENT, CAN'T              
         BZ    VR220                 HAVE PW ON EST > 14 WEEKS                  
         GOTO1 ADDAY,DMCB,SYR,WORK,98                                           
         CLC   WORK(6),EYR                                                      
         BNH   SPERREX                                                          
*                                                                               
VR220    GOTO1 DATCON,DMCB,EYR,(2,EMGDTE)  VALIDATE END DATES                   
         MVI   DMCB,0                                                           
*                                                                               
***********************************************************************         
*                                                                               
         LA    R2,ESTOWDH            OUT OF WEEK ROTATOR                        
         CLI   5(R2),0                                                          
         BNE   VR230                                                            
         CLI   SVCLEX+10,C'Y'        NOT INPUTTED, CLIENT CONTROLLED?           
         BE    VR250                 YES - GO VALIDATE                          
         B     VR260                 NO  - INPUT ON BOTH LEVELS                 
*                                                                               
VR230    MVC   ERRNUM,=AL2(ERROR)                                               
         CLI   8(R2),C'N'            NO ONE KNOWS WHY THIS TEST                 
         BE    VR260                 IS HERE.  MHER 17SEP97                     
*&&DO                                                                           
         CLI   QMED,C'R'             IF INPUT IS NOT N, MEDIA = RADIO           
         BNE   VR240                 AND ACTION = ADD                           
         CLI   ACTEQU,ACTADD         DON'T ALLOW OUT-OF-WEEK ROTATOR            
         BE    SPERREX                                                          
*&&                                                                             
VR240    CLI   8(R2),C'Y'            IF INPUT IS NOT N AND MEDIA                
         BNE   SPERREX               IS NOT RADIO AND INPUT IS Y                
*                                    DON'T ALLOW OUT-OF-WEEK ROTATOR            
*                                                                               
VR250    GOTO1 GETDAY,DMCB,(0,SYR),DUB                                          
         CLC   DUB(3),SPACES                                                    
         BE    ERRINV                                                           
         CLI   0(R1),1                                                          
         BE    ERRINV                                                           
*                                                                               
VR260    MVC   OWSDAY,DMCB                                                      
         XC    POLDATA(POLDATAX-POLDATA),POLDATA                                
*                                                                               
VR270    CLC   QPRD,=C'POL'          CHECK COMPATABILITY WITH POL/BRAND         
         BNE   VR280                                                            
         CLI   ACTEQU,ACTADD         SEE IF ADD                                 
         BE    VR300                                                            
*                                                                               
VR280    XC    KEY,KEY               POL CHANGE REREAD OLD REC                  
         MVC   KEY(8),ESTKEY                                                    
         MVC   KEY+4(3),=C'POL'                                                 
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(8),KEY                                                   
         BNE   VR350                                                            
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R5,AIO2                                                          
*                                                                               
         CLC   QPRD,=C'POL'    SEE IF CHANGING POL                              
         BE    VR290                                                            
         LA    R2,ESTSTATH                                                      
*  CANNOT SUPPORT THIS TEST ANYMORE DUE TO SPOT ESTIMATE                        
*  RECORDS ON THE NETWORK SYSTEM.                                               
*        CLC   EPRDCD(1),REC+EPRDCD-EKEY   NETPAK OLD-NEW                       
*        BNE   CKPOLERR                    MUST MATCH POL                       
         LA    R2,ESTSTRDH                                                      
         CLC   ESTART,SYR                                                       
         BNE   BRPOLERR                                                         
         LA    R2,ESTENDDH                                                      
         CLC   EEND,EYR                                                         
         BNE   BRPOLERR                                                         
         LA    R2,ESTOWDH                                                       
         CLC   EOWSDAY,OWSDAY       MUST MATCH POL                              
         BNE   BRPOLERR                                                         
*                                                                               
VR290    MVC   POLDEMOS,EDEMOS     SAVE POL DEMOS                               
         MVC   POLBOOK,EBOOK                                                    
         MVC   POLHUT,EHUTADJ                                                   
         MVC   POLDPT,EDAYMENU                                                  
         CLI   SVF0PROF,C'N'       POL=BRAND EST FILTERS                        
         BE    *+10                                                             
         MVC   POLFLTRS,EPROF      FILTERS - REMOVE PER MEL                     
         MVC   POLECON,ECONTROL                                                 
         CLI   SVF0PROF+1,C'N'     POL=BRAND RTL SCHEME                         
         BE    *+10                                                             
         MVC   POLRTL,ERTLSCHM                                                  
         MVI   POLSW,1             SET FOR EXISTENCE OF POL HDR                 
         CLC   QPRD,=C'POL'    SEE IF CHANGING POL                              
         BNE   VR350                                                            
         MVI   POLSW,2             POL EXISTED AND CHANGING POL                 
         LA    R8,REC              RESET R8 TO REC                              
* IF NO BRANDS EXIST, ALLOW POL DEMO DELETES                                    
         BAS   RE,FRSTPRD          IF NOT ALLOW POL DEMO DELETES                
         EJECT                                                                  
VR300    CLC   ESTART,SYR        SEE IF DATES WERE CHANGED                      
         BNE   VR310                                                            
         CLC   EEND,EYR                                                         
         BNE   VR310                                                            
         CLC   EOWSDAY,OWSDAY        OR EOWSDAY                                 
         BNE   VR310                                                            
         B     VR360                                                            
*                                                                               
VR310    BAS   RE,FRSTPRD          READ BRAND ESTIMATES                         
         B     *+8                                                              
*                                                                               
VR320    BAS   RE,NEXTPRD                                                       
         BNE   VR350                                                            
*                                                                               
         LA    R8,REC2                                                          
         ST    R8,AREC                                                          
         GOTO1 GETREC                                                           
*                                                                               
         CLI   SVACT,C'A'        IF POL ADD DATES MUST AGREE                    
         BNE   VR330                                                            
         LA    R2,ESTSTATH                                                      
*  CANNOT SUPPORT THIS TEST ANYMORE DUE TO SPOT ESTIMATE                        
*  RECORDS ON THE NETWORK SYSTEM.                                               
*        CLC   EPRDCD(1),REC+EPRDCD-EKEY   NETPAK OLD-NEW                       
*        BNE   BRPOLERR                    MUST MATCH POL                       
         LA    R2,ESTSTRDH                                                      
         CLC   ESTART,SYR                                                       
         BNE   BRPOLERR                                                         
         LA    R2,ESTENDDH                                                      
         CLC   EEND,EYR                                                         
         BNE   BRPOLERR                                                         
         LA    R2,ESTOWDH                                                       
         CLC   EOWSDAY,OWSDAY       EOWSDAY MUST MATCH                          
         BNE   BRPOLERR                                                         
         LA    R2,LFMRECH                                                       
         OC    EORDN(208),EORDN                                                 
         BZ    VR320                                                            
         CLI   ESTDESC,C'@'        TO GET AROUND THIS ERROR                     
         BE    VR320                                                            
         MVI   ERRCD,EDOLERR       ORDERED OR PAID DOLLARS                      
*                                  ON AN BRAND EST                              
         B     LFMERR                                                           
*                                                                               
VR330    CLC   ESTART,SYR         ON POL CHA - CHANGE BRAND DATES               
         BNE   VR340                                                            
         CLC   EEND,EYR                                                         
         BNE   VR340              NO CHANGE - GO CHECK NEXT BRAND               
         CLC   EOWSDAY,OWSDAY                                                   
         BE    VR320              NO CHANGE - GO CHECK NEXT BRAND               
*                                                                               
VR340    MVC   ESTART,SYR                                                       
         MVC   EEND,EYR                                                         
         MVC   EMGDTE,MAKEGDDT        STORE NEW MAKE GOOD DATE                  
         MVC   EOWSDAY,OWSDAY      SAVE EOWSDAY                                 
*                                     IN BRAND ESTIMATES                        
         GOTO1 PUTREC                                                           
         BAS   RE,DOCANADA                                                      
         B     VR320         UPDATED   BRAND  HDR WRITTEN BACK                  
*                                                                               
*                                                                               
VR350    MVI   WORK2,C' '     CHECK CHECK OF ALLOWING CHANGES W/O VAL           
         LA    R8,REC                   RESET ESTDHRD TO COVER REC              
         MVC   ESTART,SYR     DATES AND EOWSDAY  - OK PUT INTO REC              
         MVC   EEND,EYR                                                         
         MVC   EOWSDAY,OWSDAY                                                   
         CLC   SVKEY+4(3),=C'POL'                                               
         BNE   VR360                                                            
         CLI   SVACT,C'C'                                                       
         BE    OUTPUT        ON POL DATE CHANGES OMIT OTHER EDITS               
         EJECT                                                                  
*                                                                               
VR360    LA    R2,ESTBBASH                                                      
         XC    EBILLBAS(5),EBILLBAS                                             
         CLI   5(R2),0                                                          
         BE    VR400               WAS TO EDT2A FOR CLT PROFILE CHKS            
*                                                                               
VR370    XC    WORK,WORK                                                        
                                                                                
         MVC   WORK+16(4),=C'SB1X'   'S' MUST BE LOWER CASE - 3 CHAR            
         MVI   WORK+16,X'A2'         MAKE THE S LOWER CASE !                    
         MVC   WORK+20(2),AGENCY     PROFILE NAME                               
         MVC   WORK+22(1),QMED                                                  
         MVC   WORK+23(3),SVEBCCLT                                              
         GOTO1 GETPROF,DMCB,WORK+16,WORK,VDATAMGR                               
         CLI   WORK+11,C' '        DON'T ALLOW BILL FORMULA IF OPT 12           
         BNH   VR380               IS SET                                       
         CLI   WORK+11,C'N'                                                     
         BNE   ERRINV                                                           
*                                                                               
VR380    GOTO1 ANY                      R2 $S AT ESTBBASH                       
         XR    R4,R4                                                            
         IC    R4,5(R2)                                                         
         BCTR  R4,0                                                             
         LA    R5,8(R2)                                                         
         CLI   8(R2),C'C'          CHK FOR COMMISSION ONLY                      
         BNE   VR390                                                            
         OI    EBILLBAS,X'40'                                                   
         BCTR  R4,0                                                             
         CLI   5(R2),1             DON'T ACCEPT 'C' ALONE                       
         BE    ERRINV                                                           
         LA    R5,1(R5)            BUMP PAST 'C'                                
*                                                                               
VR390    EX    R4,GROSCOM                                                       
         BE    VR400                                                            
         EX    R4,NETCOM                                                        
         BNE   ERRINV                                                           
         OI    EBILLBAS,X'10'           CHECK PROFILE                           
*                                                                               
VR400    LA    R2,ESTCPCTH                                                      
         CLI   5(R2),0                  NOT REQUIRED                            
         BE    VR420                                                            
         XR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         BCTR  R0,R0                                                            
         GOTO1 CASHVAL,DMCB,(4,ESTCPCT+1),(R0)                                  
         CLI   DMCB,X'FF'                                                       
         BE    ERRINV                                                           
         L     R0,DMCB+4                                                        
         C     R0,=F'1000000'                                                   
         BH    ERRINV                                                           
         C     R0,=F'0'                                                         
         BNH   ERRINV                                                           
         CLI   ESTCPCT,C'+'                                                     
         BE    VR410                                                            
         CLI   ESTCPCT,C'-'                                                     
         BNE   ERRINV                   ERROR                                   
         LCR   R0,R0                    MAKE NEGATIVE                           
*                                                                               
VR410    ST    R0,FULL                                                          
         MVC   EBILLCOM,FULL                                                    
         B     VR430                                                            
*                                                                               
VR420    CLI   ESTCBASH+5,0             REQUIRED IF COM BASIS PRESENT           
         BNE   ERRMSSNG                                                         
         EJECT                                                                  
*                                                                               
VR430    LA    R2,ESTCBASH                                                      
         CLI   5(R2),0                  NOT REQUIRED                            
         BE    VR450                                                            
         XR    R4,R4                                                            
         IC    R4,5(R2)                                                         
         BCTR  R4,0                                                             
         LA    R5,8(R2)                                                         
         EX    R4,GROSCOM                                                       
         BE    VR440                                                            
         EX    R4,NETCOM                                                        
         BNE   ERRINV                                                           
         OI    EBILLBAS,X'01'                                                   
*                                                                               
VR440    B     VR460                                                            
*                                                                               
VR450    CLI   ESTCPCTH+5,0                                                     
         BNE   ERRMSSNG                                                         
*                                                                               
VR460    OC    EBILLBAS(5),EBILLBAS                                             
         BNZ   VR470                   FORMULA INPUT                            
         CLI   ESTBBASH+5,0            NO FORMULA                               
         BE    VR490                                                            
         B     VR480                                                            
*                                                                               
VR470    OC    EBILLCOM,EBILLCOM   CHK FOR COMM PCT                             
         BNZ   VR490                                                            
         CLI   EBILLBAS,X'40'      COMM ONLY - GROSS ALONE                      
         BNE   VR490                                                            
*                            FORMULA MUST HAVE BEEN GROSS ALONE                 
VR480    OI    EBILLBAS,X'80'                                                   
*                            SO BILLING WILL THINK IT'S A FORMULA               
         EJECT                                                                  
*                                                                               
VR490    LA    R2,ESTDEMSH                                                      
         XC    EDEMOS(124),EDEMOS   SO I WON'T CLEAR EOWSDAY,ERATE              
         MVI   WTSW,0              ZERO WEIGHTED DEMO INPUT SW                  
         MVI   HMSW,0              ZERO TOTAL HOMES SWITCH                      
         MVI   NHMSW,0             0 NON HOMES SWITCH                           
         CLI   5(R2),0                                                          
         BNE   VR500                                                            
         MVI   ERRCD,TV1DEM        TV MUST HAVE AT LEAST 1 DEMO                 
         CLI   QMED,C'T'                                                        
         BE    LFMERR                                                           
         CLI   OVSYS,3             SEE IF NETPAK EST                            
         BNE   VR800               NO                                           
         B     ERRTOTH                                                          
*                                                                               
VR500    MVI   ERRCD,DEMINV                                                     
         XC    REC2(250),REC2                                                   
         XC    REC2+250(50),REC2+250                                            
*                                  CHK FOR DEMO MENU                            
         CLC   8(5,R2),=C'MENU='                                                
         BNE   VR530                                                            
*                                                                               
*              INPUT IN SECOND DEMO LINE IS AN ERROR                            
         LA    R2,ESTDEM2H                                                      
         CLI   5(R2),0                                                          
         BNE   ERRINV                                                           
*                                                                               
         LA    R2,ESTDEMSH         RESET R2                                     
         ZIC   R1,5(R2)                                                         
         SH    R1,=H'5'            ADJUST FOR MENU=                             
         BNP   ERRINV                                                           
         CH    R1,=H'4'                                                         
         BH    ERRINV                                                           
         MVC   WORK2(13),KEY                                                    
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D26'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),13(R2)                                                  
         OC    KEY+3(4),SPACES                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   ERRINV                                                           
         LA    R0,REC2                                                          
         ST    R0,AREC                                                          
         GOTO1 GETREC                                                           
         LA    R5,REC2+24                                                       
         LA    R4,EDEMLST                                                       
         LA    R6,DMAX                                                          
         MVI   ELCODE,X'05'                                                     
         EJECT                                                                  
*                                                                               
VR510    BAS   RE,NEXTEL                                                        
         BNE   VR520                                                            
         MVC   0(3,R4),2(R5)                                                    
         LA    R4,3(R4)                                                         
         BCT   R6,VR510                                                         
*                                                                               
VR520    MVC   KEY(13),WORK2           RESTORE KEY                              
         B     VR550               GO DO OTHER CHKS                             
*                                                                               
VR530    LA    R5,REC2                                                          
         USING DBLOCKD,R5                                                       
         MVC   DBCOMFCS,VCOMFACS                                                
         MVC   DBFILE,=C'TPT'                                                   
         MVI   DBSELMED,C'T'                                                    
         CLI   QMED,C'R'       FOR MEDIA = RADIO                                
         BNE   *+8                                                              
         MVI   DBSELMED,C'R'                                                    
         CLI   SVAPROF+7,C'C'        SEE IF CANADIAN AGY                        
         BNE   VR540                                                            
         CLI   SVCLEX,C'U'         CANADIAN - SEE IF USING US DEMOS             
         BE    VR540               YES                                          
         MVI   DBSELMED,C'C'                                                    
*                                                                               
VR540    MVC   DMCB+4(4),=X'D9000AD9'    DEMOVAL                                
         GOTO1 CALLOV,DMCB,0                                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(2,ESTDEMSH),(14,REC2+300),(C'S',REC2),EUSRNMS         
         CLI   DMCB+4,0                                                         
         BE    ERRINV                                                           
         ZIC   R4,DMCB+4           NUMBER OF DEMOS                              
         MH    R4,=H'3'                                                         
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   EDEMLST(0),REC2+300  SO I WON'T MOVE THE X'FF'                   
         CLI   QMED,C'R'        FOR MEDIA = RADIO                               
         BNE   VR550               REPLICATE LAST RATING/IMP TYPE IF            
*                                  CHK FOR DUPLICATES                           
VR550    LA    R4,DMAX-1                                                        
         LA    R5,EDEMLST                                                       
*                                                                               
VR560    LA    R6,3(R5)                                                         
         LA    R3,DMAX-1                                                        
*                                                                               
VR570    CLI   1(R6),0                                                          
         BE    VR580               END OF DEMOS                                 
         CLC   0(3,R5),0(R6)                                                    
         BE    DUPPERR             DUPLICATE FOUND                              
         LA    R6,3(R6)                                                         
         BCT   R3,VR570                                                         
*                                                                               
VR580    LA    R5,3(R5)                                                         
         CLI   1(R5),0             END OF DEMOS                                 
         BE    VR590                                                            
         BCT   R4,VR560                                                         
         EJECT                                                                  
*                                                                               
VR590    CLI   POLSW,1                                                          
         BE    VR680                                                            
         CLI   POLSW,2                                                          
         BNE   VR760                                                            
         CLC   POLDEMOS(60),EDEMOS   DEMOS CHANGE?                              
         BE    VR760                 NO                                         
*                                                                               
         XC    POLDEMOS,POLDEMOS                                                
         BAS   RE,FRSTPRD          READ BRAND ESTIMATES TO GET DEMOS            
         B     *+8                                                              
*                                                                               
VR600    BAS   RE,NEXTPRD                                                       
         BNE   VR640                                                            
*                                                                               
         LA    R8,REC2                                                          
         ST    R8,AREC                                                          
         GOTO1 GETREC                                                           
* MAKE A LIST OF ALL BRAND DEMOS IN POLDEMOS                                    
         LA    R4,EDEMOS                                                        
         LA    R3,DMAX             FOR FIRST BCT                                
*                                                                               
VR610    LA    R5,POLDEMOS                                                      
         LA    R6,DMAX             FOR SECOND BCT                               
*                                                                               
VR620    CLC   0(3,R4),0(R5)                                                    
         BE    VR630               FOUND                                        
         CLC   0(3,R5),=3X'00'     END OF DEMOS                                 
         BNE   *+14                                                             
         MVC   0(3,R5),0(R4)                                                    
         B     VR630                                                            
         LA    R5,3(R5)            NEXT POL DEMO                                
         BCT   R6,VR620                                                         
*                                                                               
VR630    LA    R4,3(R4)                                                         
         CLC   0(3,R4),=3X'00'     END OF DEMOS                                 
         BE    VR640                                                            
         BCT   R3,VR610                                                         
         B     VR600                                                            
*                                                                               
VR640    CLC   POLDEMOS(3),=3X'00' NO BRAND ESTIMATE DEMOS                      
         BE    VR760                                                            
         LA    R8,REC                                                           
         LA    R4,POLDEMOS                                                      
         LA    R3,DMAX             FOR FIRST BCT                                
*                                                                               
VR650    LA    R5,EDEMOS                                                        
         LA    R6,DMAX             FOR SECOND BCT                               
*                                                                               
VR660    CLC   0(3,R4),0(R5)                                                    
         BE    VR670               FOUND                                        
         LA    R5,3(R5)            NEXT POL DEMO                                
         BCT   R6,VR660                                                         
         B     ERRDMPOL                                                         
*                                                                               
VR670    LA    R4,3(R4)                                                         
         CLC   0(3,R4),=3X'00'     END OF DEMOS                                 
         BE    VR760                                                            
         BCT   R3,VR650                                                         
         B     VR760                                                            
*                                                                               
* NOW BE SURE BRAND POL DEMOS ARE A SUBSET OF POL DEMOS                         
VR680    CLC   POLDEMOS(3),=3X'00' NO POL DEMOS                                 
         BE    ERRDMPOL                                                         
         MVI   UDSW,0              ZERO USER DEMO SW                            
         LA    R4,EDEMOS                                                        
         LA    R3,DMAX             FOR FIRST BCT                                
*                                                                               
VR690    LA    R5,POLDEMOS                                                      
         LA    R6,DMAX             FOR SECOND BCT                               
         CLI   1(R4),X'21'         SEE IF LOOKING FOR A USER DEMO               
         BNE   *+8                                                              
         MVI   UDSW,1              SET USER DEMO ENCOUNTERED SW                 
*                                                                               
VR700    CLC   0(3,R4),0(R5)                                                    
         BE    VR710               FOUND                                        
         LA    R5,3(R5)            NEXT POL DEMO                                
         BCT   R6,VR700                                                         
         B     ERRDMPOL                                                         
*                                                                               
VR710    LA    R4,3(R4)                                                         
         CLC   0(3,R4),=3X'00'     END OF DEMOS                                 
         BE    VR720                                                            
         BCT   R3,VR690                                                         
*                                                                               
VR720    CLI   UDSW,0              SEE IF USING USER DEMOS                      
         BE    VR750               NO                                           
*                                                                               
         LA    R3,4                                                             
         LA    R4,EUSRNMS          BRAND USER NAMES MUST MATCH                  
         LA    R5,PEUSRNMS         POL USER NAMES                               
*                                                                               
VR730    CLI   0(R4),C' '          SEE IF BRAND USED                            
         BNH   VR740               NO SKIP                                      
         CLC   0(7,R4),0(R5)                                                    
         BNE   ERRDMPOL            ERROR DON'T MATCH                            
*                                                                               
VR740    LA    R4,7(R4)                                                         
         LA    R5,7(R5)                                                         
         BCT   R3,VR730                                                         
*                                                                               
VR750    CLI   EWGTNM,C' '         CHK WEIGHTED DEMO                            
         BNH   VR760                                                            
         CLC   EWGTNM,PEWGTNM                                                   
         BNE   ERRDMPOL            WGT DEMO MUST MATCH                          
*                                                                               
VR760    LA    R8,REC              POINT R8 TO CURRENT ESTHDR                   
         CLI   QMED,C'N'       NETWORK                                          
         BNE   VR800                                                            
*                                                                               
         CLI   SVAPROF+7,C'C'      CANADA                                       
         BE    VR800                                                            
*                                                                               
         CLI   OVSYS,3             SEE IF NETPAK EST                            
         BNE   VR800               NO                                           
         LA    R4,EDEMLST          SEE IF TOTAL HMS INPUT                       
         LA    R3,DMAX                                                          
*                                                                               
VR770    CLC   0(3,R4),=3X'00'     END OF LIST                                  
         BE    VR780                                                            
         CLC   1(2,R4),=X'C901'    TOTAL HMS                                    
         BNE   *+12                                                             
         MVI   HMSW,1                                                           
         B     *+8                                                              
         MVI   NHMSW,1             SET A NON-TOTAL HMS DEMO INPUT               
         LA    R4,3(R4)            TOTAL HMS REQUIRED FOR NETPAK                
         BCT   R3,VR770                                                         
*                                  TOTAL HMS REQUIRED FOR NETPAK                
VR780    CLI   HMSW,1                                                           
         BNE   ERRTOTH                                                          
*                                                                               
VR790    CLI   NHMSW,1             A NOT HOMES DEMO MUST BE INPUT FOR           
         BE    VR800               NETPAK                                       
         MVI   ERRCD,DEMINV                                                     
         B     LFMERR                                                           
         EJECT                                                                  
*              WEIGHT EDIT                                                      
VR800    LA    R2,ESTWTSH          DEMO WEIGHTS                                 
         CLI   5(R2),0                                                          
         BE    VR930                                                            
         LA    R3,REC2                                                          
         LA    R4,8                                                             
*                                                                               
VR810    XC    0(250,R3),0(R3)                                                  
         LA    R3,250(R3)                                                       
         BCT   R4,VR810                                                         
*                                  BUILD SCANNER TABLE OF DEMOS                 
         LA    R2,ESTDEMSH                                                      
         GOTO1 SCANNER,DMCB,(R2),(15,REC2+1000)                                 
         CLI   DMCB+4,0                                                         
         BE    ERRINV                                                           
         CLI   DMCB+4,DMAX                                                      
         BH    ERRINV                                                           
         LA    R2,ESTDEM2H                                                      
         CLI   5(R2),0                                                          
         BE    VR820                                                            
         ZIC   R4,DMCB+4                                                        
         MH    R4,=H'32'                                                        
         LA    R5,REC2+1000                                                     
         AR    R5,R4                                                            
         ZIC   R3,DMCB+4                                                        
         LA    R4,DMAX+1                                                        
         SR    R4,R3                                                            
         GOTO1 VSCANNER,DMCB,(R2),((R4),(R5))                                   
         CLI   DMCB+4,0                                                         
         BE    ERRINV                                                           
*                                                                               
VR820    LA    R2,ESTWTSH                                                       
         L     RF,COMFACS         NEED TO USE PARSNIP B/C DEMOS>10 LEN          
         L     RF,PARSNIP-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(R2),(15,REC2+500),0                                   
         MVI   ERRCD,WGHTINV                                                    
         CLI   DMCB+4,0            PARSNIP ERROR                                
         BE    LFMERR                                                           
         CLI   DMCB+4,DMAX*2                                                    
         BH    LFMERR                                                           
*                                                                               
         BAS   RE,PARTOSCN         REFORM PARSNIP BLK TO BE SCANNERISH          
*                                                                               
         LA    R3,DMAX             FOR BCT                                      
         LA    R5,REC2                                                          
         USING BSCAND,R5           BIG SCANNER DSECT                            
*                                                                               
VR830    CLC   0(2,R5),=X'0000'    LAST LINE                                    
         BE    VR930                                                            
*            FIND DEMO AND STORE WEIGHT OR TARGET                               
         ST    R3,FULL             SAVE SCANNER BCT                             
         LA    R4,EWGTLST                                                       
         LA    R6,DMAX                                                          
         LA    R7,EDEMLST                                                       
         LA    R3,REC2+1000           LOOK FOR DEMO IN DEMO SCANNER TBL         
         EJECT                                                                  
*                                                                               
VR840    CLC   BFLD1,12(R3)                                                     
         BE    VR860                                                            
         CLI   BFLD1LEN,2           TO LET THEM INPUT UN=                       
         BNE   VR850                                                            
         CLI   BFLD1,C'U'                                                       
         BNE   VR850                                                            
         CLC   BFLD1(2),12(R3)       MATCH UN                                   
         BE    VR860                                                            
*                                                                               
VR850    LA    R3,32(R3)                                                        
         LA    R4,1(R4)                                                         
         LA    R7,3(R7)                                                         
         BCT   R6,VR840                                                         
         B     LFMERR                                                           
*                                                                               
VR860    CLI   BFLD2,C'T'           SEE IF A TARGET                             
         BNE   VR890                                                            
         CLI   BFLD2LEN,2                                                       
         BNE   ERRINV                                                           
         LA    R1,ETRGLST                                                       
         CLC   BFLD2(2),=C'T1'                                                  
         BE    VR870                                                            
         LA    R1,ETRGLST+3                                                     
         CLC   BFLD2(2),=C'T2'                                                  
         BNE   ERRINV                                                           
*                                                                               
VR870    OC    0(3,R1),0(R1)                                                    
         BNZ   DUPPERR                                                          
*                                                                               
VR880    MVC   0(3,R1),0(R7)       MOVE 3 BYTE CODE FROM EDEMLIST               
         B     VR920               GO DO NEXT SCANNER LINE                      
*                                                                               
VR890    CLI   0(R4),0             SEE IF WEIGHT ALREADY THERE                  
         BNE   DUPPERR                                                          
         EJECT                                                                  
*                                                                               
VR900    CLI   1(R7),C'R'          RATINGS CAN'T HAVE WEIGHT                    
         BNE   VR910                                                            
         MVI   ERRCD,NORTWTS                                                    
         B     LFMERR                                                           
*                                                                               
VR910    L     R1,BFLD2B                                                        
         CLI   BFLD2LEN,0                                                       
         BE    LFMERR               WEIGHT INVALID OR MISSING                   
         TM    BFLD2VAL,X'80'        TEST NUMERIC                               
         BZ    LFMERR                                                           
         CH    R1,=H'255'                                                       
         BH    LFMERR                                                           
         LTR   R1,R1                                                            
         BZ    LFMERR                                                           
         STC   R1,0(R4)                                                         
         CLI   POLSW,1                                                          
         BNE   VR920                                                            
         CLC   POLDEMOS(3),=3X'00'                                              
         BE    VR920               NO POL DEMOS                                 
         BAS   RE,CKPOLWTS                                                      
*                                                                               
VR920    DS    0H                                                               
         LA    R5,BSCANLNQ(R5)     NEXT BIG SCANNER LINE                        
         L     R3,FULL             RESTORE SCANNER BCT                          
         BCT   R3,VR830                                                         
         DROP  R5                                                               
*                                                                               
VR930    OC    EWGTLST,EWGTLST                                                  
         BZ    VR940                                                            
         MVI   ERRCD,NOWTDEM       WEIGHTS BUT NO WEIGHTED DEMO                 
         CLI   EWGTNM,C' '                                                      
         BNH   LFMERR                                                           
         B     VR950                                                            
*                                                                               
VR940    CLI   EWGTNM,C' '         WEIGHTED DEMO BUT NO WEIGHTS                 
         BNH   VR950                                                            
         MVI   ERRCD,NOWTS                                                      
         B     LFMERR                                                           
*                                                                               
VR950    CLI   EMSTRIND,0          TEST MASTER OR SUB ESTIMATE                  
         BE    VR970               NO                                           
         CLC   SVDEMOS(60),EDEMLST  TEST SAME DEMOS                             
         BNE   VR960                NO - THEY SHOULD BE !                       
         CLC   SVDEMOS+80(28),EUSRNMS  TEST SAME USER NAMES                     
         BNE   VR960                   NO - SHOULD BE THE SAME                  
         B     VR970                   WEIGHTS CAN DIFFER (SEE CANDICE)         
VR960    LA    R2,ESTDEMSH         SET CURSOR TO DEMOS                          
         B     ERRNOCHG                                                         
*                                                                               
VR970    MVI   POLCHG,0                                                         
         CLI   POLSW,2             SEE IF CHANGING POL                          
         BNE   VR980                                                            
         CLC   SVUSRNMS,EUSRNMS    SEE IF CHANGING USER NAMES                   
         BE    VR980                                                            
         MVC   SVUSRNMS,EUSRNMS                                                 
         MVI   POLCHG,1                                                         
         EJECT                                                                  
*                                                                               
VR980    LA    R2,ESTRBKH            RATING BOOK                                
         GOTO1 ANY                   REQUIRED                                   
         XC    EBOOK,EBOOK                                                      
         CLI   OVSYS,3               SEE IF NETPAK EST                          
         BNE   VR990                                                            
         CLI   8(R2),C'0'                                                       
         BL    ERRINV                                                           
         B     VR1000                                                           
*                                                                               
VR990    CLC   8(6,R2),=C'LATEST'                                               
         BE    VR1010                                                           
*                                                                               
VR1000   GOTO1 DATVAL,DMCB,(2,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    DTERR                                                            
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK+10)                                 
         MVC   EBOOK,WORK+10                      YM                            
*                                                                               
VR1010   CLI   EMSTRIND,0                                                       
         BE    VR1020                                                           
         CLC   SVBOOK,EBOOK                                                     
         BE    VR1020                                                           
         CLC   QPRD,=C'POL'    ALLOW POL BOOK CHANGES                           
         BNE   ERRNOCHG                                                         
*                                                                               
VR1020   CLI   POLSW,1                                                          
         BNE   VR1030                                                           
         CLC   POLBOOK,EBOOK                                                    
         BNE   BRPOLERR                                                         
*                                                                               
VR1030   DS    0H                                                               
         CLC   QPRD,=C'POL'                                                     
         BNE   VR1050                                                           
         CLI   SVACT,C'A'          ON POL ADDS SET POLCHG TO 1                  
         BE    VR1040                                                           
         CLC   SVBOOK,EBOOK        SEE IF BOOK CHANGED                          
         BE    VR1050              NO                                           
*                                  ON POL BOOK CHANGES - GO CHANGE              
*                                  BRANDS                                       
VR1040   MVC   SVBOOK,EBOOK        SAVE POL BOOK                                
         MVI   POLCHG,1            SET POL CHANGE SWITCH                        
         EJECT                                                                  
*                                                                               
VR1050   LA    R2,ESTHUTH          HUT ADJUSTMENT                               
         GOTO1 ANY                                                              
         MVI   EHUTADJ,0                                                        
         CLC   8(4,R2),=C'AUTO'                                                 
         BE    VR1060                                                           
         CLI   5(R2),3                                                          
         BNE   ERRINV                                                           
         MVC   WORK(3),8(R2)                                                    
         MVC   WORK+3(3),=C'/77'                                                
         GOTO1 DATVAL,DMCB,(2,WORK),WORK+10                                     
         OC    DMCB(4),DMCB                                                     
         BZ    DTERR                                                            
         PACK  DUB,WORK+12(2)      MTH                                          
         CVB   R0,DUB                                                           
         SLL   R0,4                                                             
         STC   R0,EHUTADJ                                                       
*                                                                               
VR1060   CLI   EMSTRIND,0                                                       
         BE    VR1070                                                           
         CLC   SVHUT,EHUTADJ                                                    
         BE    VR1070                                                           
         CLC   QPRD,=C'POL'    ALLOW POL HUT CHANGES                            
         BNE   ERRNOCHG                                                         
*                                                                               
VR1070   CLI   POLSW,1                                                          
         BNE   VR1080                                                           
         CLC   POLHUT,EHUTADJ                                                   
         BNE   BRPOLERR                                                         
*                                                                               
VR1080   CLC   QPRD,=C'POL'                                                     
         BNE   VR1090                                                           
         CLC   SVHUT,EHUTADJ                                                    
         BE    VR1090                                                           
         MVC   SVHUT,EHUTADJ                                                    
         MVI   POLCHG,1                                                         
*                                                                               
VR1090   LA    R2,ESTMENUH              DPT MENU                                
         GOTO1 ANY                                                              
         CLI   5(R2),1                                                          
         BNE   ERRINV                   1 CHARACTER                             
         MVC   EDAYMENU,8(R2)                                                   
         MVC   DMCB(2),AGENCY                                                   
         MVC   DMCB+2(1),QMED                                                   
         MVC   DMCB+3(1),EDAYMENU                                               
         GOTO1 VDPTRD,DMCB,,REC2,VDATAMGR                                       
         MVI   ERRCD,NOFNDERR            MENU NOT ON FILE                       
         CLI   DMCB+8,X'FF'                                                     
         BE    LFMERR                                                           
         CLI   EMSTRIND,0                                                       
         BE    VR1100                                                           
         CLC   SVDPT,EDAYMENU                                                   
         BNE   ERRNOCHG                                                         
         EJECT                                                                  
*                                                                               
VR1100   CLI   POLSW,1                                                          
         BNE   VR1110                                                           
         CLC   POLDPT,EDAYMENU                                                  
         BE    VR1120                                                           
         B     BRPOLERR                                                         
*                                                                               
VR1110   CLC   QPRD,=C'POL'                                                     
         BNE   VR1120                                                           
         CLC   SVDPT,EDAYMENU                                                   
         BE    VR1120                                                           
         MVC   SVDPT,EDAYMENU                                                   
         MVI   POLCHG,1                                                         
*                                                                               
VR1120   LA    R2,ESTFLTRH         FILTERS                                      
         XC    EPROF(3),EPROF                                                   
         CLI   5(R2),0             NO INPUT                                     
         BE    VR1150                                                           
         OC    8(3,R2),SPACES                                                   
         CLC   8(3,R2),SPACES      TREAT BLANKS AS NO INPUT                     
         BE    VR1150                                                           
         MVC   EPROF(3),ESTFLTR                                                 
         LA    R4,EPROF                                                         
         LA    R5,3                                                             
*                                                                               
VR1130   CLI   0(R4),C' '                                                       
         BE    VR1140                                                           
         CLI   0(R4),C'A'                                                       
         BL    ERRINV                                                           
         CLI   0(R4),C'9'                                                       
         BH    ERRINV                                                           
*                                                                               
VR1140   LA    R4,1(R4)                                                         
         BCT   R5,VR1130                                                        
*                                                                               
VR1150   CLI   SVCLEX+3,C'Y'       SEE IF FILTERS REQUIRED                      
         BNE   VR1160              NO                                           
         OC    EPROF(3),EPROF                                                   
         BZ    ERRMSSNG                                                         
*                                                                               
VR1160   DS    0H                                                               
         CLI   SVF0PROF+2,C'Y'    FILTER 1 REQUIRED                             
         BNE   VR1170                                                           
         MVI   ERRCD,FLT1MSS                                                    
         CLI   EPROF,C' '                                                       
         BNH   LFMERR                                                           
*                                                                               
VR1170   CLI   SVF0PROF+3,C'Y'    FILTER 2 REQUIRED                             
         BNE   VR1180                                                           
         MVI   ERRCD,FLT2MSS                                                    
         CLI   EPROF+1,C' '                                                     
         BNH   LFMERR                                                           
*                                                                               
VR1180   CLI   SVF0PROF+4,C'Y'    FILTER 3 REQUIRED                             
         BNE   VR1190                                                           
         MVI   ERRCD,FLT3MSS                                                    
         CLI   EPROF+2,C' '                                                     
         BNH   LFMERR                                                           
*                                                                               
VR1190   DS    0H                                                               
         CLI   SVF0PROF,C'N'       POL=BRAND EST FILTERS                        
         BE    VR1210                                                           
         CLI   POLSW,1            IF POL EST EXISTS                             
         BNE   VR1200                                                           
         CLC   POLFLTRS,EPROF     & ESTIMATE FILTERS ARE DIFFERENT              
         BE    VR1210                                                           
         CLI   QMED,C'R'      & RADIO                                           
         BNE   BRPOLERR                                                         
         CLI   SVCLPROF,C'0'      & BRAND POL (1 & 2)                           
         BE    BRPOLERR           - THEN DIFF OKAY (CHK AT VR1380 TOO)          
         EJECT                                                                  
*                                                                               
VR1200   CLC   QPRD,=C'POL'                                                     
         BNE   VR1210                                                           
         CLC   SVFLTRS,EPROF                                                    
         BE    VR1210                                                           
         MVC   SVFLTRS,EPROF                                                    
         MVI   POLCHG,1                                                         
*                                                                               
VR1210   LA    R2,ESTECONH    ** ECONTROL                                       
         MVI   ECONTROL,0                                                       
         CLI   5(R2),0                                                          
         BE    VR1240                                                           
*                                                                               
         OC    ESTECON,SPACES                                                   
         CLC   ESTECON(2),=C'E '                                                
         BNE   VR1230                                                           
         MVI   ECONTROL,EBILESTQ                                                
*                        ECONTROL 'E' ONLY FOR CERTAIN AGYS                     
*                                                                               
         LA    R1,ECTAGY                                                        
*                                                                               
VR1220   CLI   0(R1),X'FF'                                                      
         BE    ERRINV                                                           
         CLC   0(2,R1),AGENCY                                                   
         BE    VR1240                                                           
         LA    R1,2(R1)                                                         
         B     VR1220                                                           
*                                                                               
VR1230   CLC   ESTECON(3),=C'NSC'  'NO SEPARATE COMMISSION'                     
         BNE   ERRINV                                                           
         MVI   ECONTROL,ENSEPCMQ                                                
*                                                                               
VR1240   CLI   SVACT,C'A'          SEE IF ADD                                   
         BE    VR1270                                                           
         BAS   RE,CKLINID          TEST TERMINAL AUTHORIZED                     
         BE    VR1270              ALLOW CHG OF OLD-NEW                         
         CLC   SVECON,ECONTROL     SEE IF ECONTROL CHANGED                      
         BE    VR1270              NO - SKIP BILLING CHECKS                     
         XC    KEY,KEY             MUST CHECK FOR BILLING RECORDS               
         MVC   KEY(9),SVKEY                                                     
         GOTO1 HIGH                                                             
         GOTO1 SEQ                                                              
         CLC   KEY(8),KEYSAVE                                                   
         BE    ERRNOCHG            MEANS NO BILLS FOUND                         
         EJECT                                                                  
*                                                                               
VR1250   DS    0H                  IF CHANGING POL                              
*                                  I MUST CHECK FOR BILLING FOR ANY             
*                                  BRAND                                        
         CLC   QPRD,=C'POL'                                                     
         BNE   VR1270                                                           
         BAS   RE,FRSTPRD                                                       
         B     *+8                                                              
*                                                                               
VR1260   BAS   RE,NEXTPRD                                                       
         BNE   VR1270              DONE                                         
*                                                                               
         GOTO1 SEQ                                                              
         CLC   KEY(8),KEYSAVE      SEE IF I FOUND BILL                          
         BE    ERRNOCHG            BILL FOUND - ERROR                           
         B     VR1260              GO TRY NEXT PRODUCT                          
*                                                                               
VR1270   CLI   POLSW,1             CHECK VERSES POL DATA                        
         BNE   VR1280                                                           
         CLC   POLECON,ECONTROL                                                 
         BE    VR1290                                                           
         B     BRPOLERR                                                         
*                                                                               
VR1280   CLC   QPRD,=C'POL'                                                     
         BNE   VR1290                                                           
         CLC   SVECON,ECONTROL                                                  
         BE    VR1290                                                           
         MVC   SVECON,ECONTROL                                                  
         MVI   POLCHG,1                                                         
         EJECT                                                                  
*                                                                               
VR1290   DS    0H                                                               
         XC    ERTLSCHM,ERTLSCHM                                                
         LA    R2,ESTERTLH                                                      
         CLI   5(R2),0                                                          
         BE    *+10                                                             
         MVC   ERTLSCHM,8(R2)                                                   
*                                                                               
         CLI   SVF0PROF+1,C'N'     POL=BRAND RTL SCHEME                         
         BE    VR1310                                                           
         CLI   POLSW,1            IF POL EST EXISTS                             
         BNE   VR1300                                                           
         CLC   POLRTL,ERTLSCHM    & RETAIL SCHEME DIFFERENT                     
         BE    VR1310                                                           
         CLI   QMED,C'R'      & RADIO                                           
         BNE   BRPOLERR                                                         
         CLI   SVCLPROF,C'0'      & BRAND POL (1 & 2)                           
         BE    BRPOLERR           - THEN DIFF OKAY (CHK AT VR1380  TOO)         
*                                                                               
VR1300   CLC   QPRD,=C'POL'                                                     
         BNE   VR1310                                                           
         CLC   SVRTL,ERTLSCHM                                                   
         BE    VR1310                                                           
         MVC   SVRTL,ERTLSCHM                                                   
         MVI   POLCHG,1                                                         
*                                                                               
VR1310   XC    WORK,WORK                                                        
         OC    SVE1USER,SVE1USER   ANY "ESTIMATE 1" INFO                        
         BZ    VR1320                                                           
         LA    R2,ESTUSR1H         A(INPUT)                                     
         MVC   HALF(1),SVE1TYPE    TYPE                                         
         MVC   HALF+1(1),SVE1FLG1  FLAG                                         
         MVC   BYTE,SVE1LEN        LENGTH                                       
         GOTO1 =A(EDTUSR),RR=RELO                                               
*                                                                               
VR1320   MVC   EUSER1,WORK                                                      
         MVC   ESTUSR1,WORK        RE-TRANSMIT FIELD                            
         OI    ESTUSR1H+6,X'80'                                                 
*                                                                               
         XC    WORK,WORK                                                        
         OC    SVE2USER,SVE2USER   ANY "ESTIMATE 2" INFO                        
         BZ    VR1330                                                           
         LA    R2,ESTUSR2H         A(INPUT FIELD)                               
         MVC   HALF(1),SVE2TYPE    TYPE                                         
         MVC   HALF+1(1),SVE2FLG1  FLAG                                         
         MVC   BYTE,SVE2LEN        LENGTH                                       
         GOTO1 =A(EDTUSR),RR=RELO                                               
*                                                                               
VR1330   MVC   EUSER2,WORK                                                      
         MVC   ESTUSR2,WORK        CLEAR OR RE-TRANSMIT FIELD.                  
         OI    ESTUSR2H+6,X'80'                                                 
*                                                                               
         GOTO1 =A(VALPROF),RR=RELO                                              
         BE    VR1340                                                           
         LA    R2,ESTOPTH          SET R2 TO FIELD W/ ERROR                     
         CLI   DMCB,1                                                           
         BE    ERRINV                                                           
         CLI   DMCB,2                                                           
         BE    DLYERR                                                           
         CLI   DMCB,3                                                           
         BE    REQERR                                                           
         DC    H'0'                                                             
*                                                                               
VR1340   CLC   QPRD,=C'POL'                                                     
         BNE   EDT10                                                            
         CLI   POLCHG,0            SEE IF NEED TO CHANGE BRAND ESTS             
         BE    EDT10               NO                                           
*                                                                               
         BAS   RE,FRSTPRD          READ BRAND ESTIMATES                         
         B     *+8                                                              
*                                                                               
VR1350   BAS   RE,NEXTPRD                                                       
         BNE   VR1510              DONE                                         
*                                                                               
         LA    R8,REC2             NOTE - ESTHDRD NOW COVERS REC2               
         ST    R8,AREC                                                          
         GOTO1 GETREC                                                           
         CLI   SVACT,C'A'                                                       
         BNE   VR1430                                                           
* ON POL ADDS BOOK MUST AGREE WITH BRAND ESTIMATE                               
         CLC   EBOOK,SVBOOK                                                     
         BE    VR1360                                                           
         LA    R2,ESTRBKH          CURSOR TO BOOK                               
         B     BRPOLERR                                                         
*                                                                               
VR1360   CLC   EHUTADJ,SVHUT                                                    
         BE    VR1370                                                           
         LA    R2,ESTHUTH                                                       
         B     LFMERR                                                           
*                                                                               
VR1370   CLC   EDAYMENU,SVDPT                                                   
         BE    VR1380                                                           
         LA    R2,ESTMENUH                                                      
         B     BRPOLERR                                                         
*                                                                               
VR1380   DS    0H                                                               
         CLI   SVF0PROF,C'N'       POL=BRAND EST FILTERS                        
         BE    VR1400                                                           
         CLC   EPROF(3),SVFLTRS   IF ESTIMATE FILTERS DIFFERENT                 
         BE    VR1400                                                           
         CLI   QMED,C'R'      & RADIO                                           
         BNE   VR1390                                                           
         CLI   SVCLPROF,C'0'      & BRAND POL (1 OR 2)                          
         BNE   VR1400             - THEN DIFFERENCE OKAY                        
*                                                                               
VR1390   LA    R2,ESTFLTRH                                                      
         B     LFMERR                                                           
         EJECT                                                                  
*                                                                               
VR1400   CLC   ECONTROL,SVECON                                                  
         BE    VR1410                                                           
         LA    R2,ESTECONH                                                      
         B     LFMERR                                                           
*                                                                               
VR1410   CLI   SVF0PROF+1,C'N'     POL=BRAND RTL SCHEME                         
         BE    VR1350                                                           
         CLC   ERTLSCHM,SVRTL                                                   
         BE    VR1350                                                           
         CLI   QMED,C'R'      & RADIO                                           
         BNE   VR1420                                                           
         CLI   SVCLPROF,C'0'      & BRAND POL (1 OR 2)                          
         BNE   VR1350             - THEN DIFFERENCE OKAY                        
*                                                                               
VR1420   LA    R2,ESTERTLH                                                      
         B     LFMERR                                                           
*                                                                               
VR1430   CLC   EBOOK,SVBOOK        SEE IF I NEED TO CHG BRAND                   
         BNE   VR1480                                                           
         CLC   EPWPCT,SVPOLPW                                                   
         BNE   VR1480                                                           
         CLC   EHUTADJ,SVHUT                                                    
         BNE   VR1480                                                           
         CLC   EDAYMENU,SVDPT                                                   
         BNE   VR1480                                                           
         CLI   SVF0PROF,C'N'       POL=BRAND EST FILTERS                        
         BE    VR1440                                                           
         CLC   EPROF(3),SVFLTRS                                                 
         BNE   VR1480                                                           
*                                                                               
VR1440   CLC   ECONTROL,SVECON                                                  
         BNE   VR1480                                                           
         CLI   SVF0PROF+1,C'N'     POL=BRAND RTL SCHEME                         
         BE    VR1450                                                           
         CLC   ERTLSCHM,SVRTL                                                   
         BNE   VR1480                                                           
*                                                                               
VR1450   LA    R4,4                FOR BCT                                      
         LA    RF,EUSRNMS                                                       
         LA    RE,SVUSRNMS                                                      
*                                                                               
VR1460   CLI   0(RF),C' '          SEE IF NAME USED                             
         BNH   VR1470                                                           
         CLC   0(7,RF),0(RE)       YES THEN MUST MATCH                          
         BNE   VR1480                                                           
*                                                                               
VR1470   LA    RF,7(RF)                                                         
         LA    RE,7(RE)                                                         
         BCT   R4,VR1460                                                        
         B     VR1350                                                           
*                                                                               
VR1480   MVC   EHUTADJ,SVHUT                                                    
         MVC   EPWPCT,SVPOLPW                                                   
         MVC   EBOOK,SVBOOK                                                     
         MVC   EDAYMENU,SVDPT                                                   
         CLI   SVF0PROF,C'N'       POL=BRAND EST FILTERS                        
         BE    *+10                                                             
         MVC   EPROF(3),SVFLTRS                                                 
         MVC   ECONTROL,SVECON                                                  
         CLI   SVF0PROF+1,C'N'     POL=BRAND RTL SCHEME                         
         BE    *+10                                                             
         MVC   ERTLSCHM,SVRTL                                                   
         LA    R4,4                FOR BCT                                      
         LA    RF,EUSRNMS                                                       
         LA    RE,SVUSRNMS                                                      
         EJECT                                                                  
*                                                                               
VR1490   CLI   0(RF),C' '          SEE IF NAME USED                             
         BNH   VR1500                                                           
         MVC   0(7,RF),0(RE)       STORE NEW USER NAME                          
*                                                                               
VR1500   LA    RF,7(RF)                                                         
         LA    RE,7(RE)                                                         
         BCT   R4,VR1490                                                        
         GOTO1 PUTREC                                                           
         BAS   RE,DOCANADA                                                      
         B     VR1350                                                           
*                                                                               
VR1510   LA    R8,REC                  RESET R8 TO REC                          
*                                      NOTE - ESTHDRD NOW COVERS REC            
         B     EDT10                                                            
***********************************************************************         
*        END VALREC AND CALL DISPREC                                            
***********************************************************************         
*                                                                               
VRX      OI    GENSTAT2,RETEQSEL                                                
         B     DR                    REDISPLAY RECORD                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*                       DISPLAY RECORD                                *         
***********************************************************************         
*                                                                               
DR       LA    R0,ESTOPTH           LAST FIELD                                  
         LA    R2,ESTDESCH          FIRST FIELD                                 
*                                                                               
DR01     ZIC   R1,0(R2)             LENGTH OF FIRST FIELD                       
         SH    R1,=H'9'             MINUS HEADER AND 1 FOR EX                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES       BLANK CURRENT FIELD                         
         OI    6(R2),X'80'          TRANSMIT                                    
DR02     ZIC   R1,0(R2)             R1 = LENGTH OF FIELD + HEADER               
         AR    R2,R1                NEXT FIELD                                  
         CR    R2,R0                END OF SCREEN?                              
         BH    *+16                                                             
         TM    1(R2),X'20'          NO, IS FIELD PROTECTED?                     
         BZ    DR01                 NO, CLEAR IT                                
         B     DR02                 YES, BUMP TO NEXT FIELD                     
*                                                                               
***********************************************************************         
*                                                                               
         USING ESTHDR,R3            ESTIMATE RECORD                             
         L     R3,AIO                                                           
*                                                                               
***********************************************************************         
*                                                                               
*        TM    EPRDCD,X'80'          SEE IF NEW NETPAK ESTHDR                   
*        BZ    *+10                                                             
*        MVC   LFMKEXP+50(2),=C' N'                                             
*        FOUT  LFMKEXPH                                                         
*                                                                               
         MVC   ESTDESC,EDESC         DESCRIPTION                                
         OI    ESTDESCH+6,X'80'                                                 
*                                                                               
**********************************************************************          
*                                                                               
         TM    ECNTRL,X'04'                                                     
         BZ    DR10                                                             
         MVC   ESTSTAT,=CL8'HOLD'                                               
         OI    ESTSTATH+6,X'80'                                                 
         B     DR40                                                             
*                                                                               
DR10     TM    ECNTRL,X'08'                                                     
         BZ    DR20                                                             
         MVC   ESTSTAT,=CL8'CLOCK'                                              
         OI    ESTSTATH+6,X'80'                                                 
         B     DR40                                                             
*                                                                               
DR20     OC    ELOCKYM,ELOCKYM                                                  
         BZ    DR30                                                             
         MVC   ESTSTAT,SPACES                                                   
         MVC   WORK(L'ELOCKYM),ELOCKYM     MOVE YM TO WORK                      
         NI    WORK+1,X'FF'-X'80'-X'40'    TURN OFF FLAG BITS                   
         GOTO1 DATCON,DMCB,(3,WORK),(6,ESTSTAT)                                 
         TM    ELOCKMON,X'80'                                                   
         BZ    *+8                                                              
         MVI   ESTSTAT+6,C'-'              TEST IF PRIOR                        
         TM    ELOCKMON,X'40'                                                   
         BZ    *+8                                                              
         MVI   ESTSTAT+6,C'+'              TEST IF SUBSEQUENT                   
         OI    ESTSTATH+6,X'80'                                                 
         B     DR40                                                             
*                                                                               
DR30     MVC   ESTSTAT,SPACES                                                   
         OI    ESTSTATH+6,X'80'                                                 
*                                                                               
**********************************************************************          
*                                                                               
DR40     GOTO1 DATCON,DMCB,(0,ESTART),(5,ESTSTRD)                               
         OI    ESTSTRDH+6,X'80'                                                 
*                                                                               
**********************************************************************          
*                                                                               
         GOTO1 DATCON,DMCB,(0,EEND),(5,ESTENDD)                                 
         OI    ESTENDDH+6,X'80'                                                 
*                                                                               
**********************************************************************          
*                                                                               
         OI    ESTBBASH+6,X'80'                                                 
         OI    ESTCPCTH+6,X'80'                                                 
         OI    ESTCBASH+6,X'80'                                                 
*                                                                               
         OC    EBILLBAS(5),EBILLBAS                                             
         BNZ   DR50                     NO BILLING FORMULA                      
         MVC   ESTBBAS,SPACES                                                   
         MVC   ESTCPCT,SPACES                                                   
         MVC   ESTCBAS,SPACES                                                   
         B     DR100                                                            
*                                                                               
DR50     MVC   ESTBBAS,=CL5'CNET'                                               
         TM    EBILLBAS,X'50'                                                   
         BO    DR60                                                             
*                                                                               
         MVC   ESTBBAS,=CL5'NET'                                                
         TM    EBILLBAS,X'10'                                                   
         BO    DR60                                                             
*                                                                               
         MVC   ESTBBAS,=C'CGROS'                                                
         TM    EBILLBAS,X'40'                                                   
         BO    DR60                                                             
*                                                                               
         MVC   ESTBBAS,=C'GROSS'                                                
*                                                                               
DR60     L     R4,EBILLCOM                                                      
         LTR   R4,R4                                                            
         BNZ   DR70                                                             
         MVC   ESTCPCT,SPACES                                                   
         MVC   ESTCBAS,SPACES                                                   
         B     DR100                                                            
*                                                                               
DR70     LPR   RF,R4                                                            
         C     RF,=F'1000000'      +/-100.0000 WONT FIT                         
         BNE   DR80                                                             
         MVC   ESTCPCT+1(3),=C'100'                                             
         B     DR90                                                             
*                                                                               
DR80     EDIT  (R5),(8,ESTCPCT),4,FLOAT=+,ALIGN=LEFT                            
*                                                                               
DR90     LTR   R4,R4                                                            
         BNM   *+8                                                              
         MVI   ESTCPCT,C'-'                                                     
         MVC   ESTCBAS,=CL5'NET'                                                
         TM    EBILLBAS,X'01'                                                   
         BO    *+10                                                             
         MVC   ESTCBAS,=C'GROSS'                                                
*                                                                               
**********************************************************************          
*                                                                               
DR100    XC    ESTDEMS,ESTDEMS          FORMAT DEMOS                            
         XC    ESTDEM2,ESTDEM2          CLEAR LINE 2                            
         OC    EDEMLST(3),EDEMLST  SEE IF I HAVE ANY DEMOS                      
         BZ    DR150                                                            
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING DBLOCK,R4                                                        
         MVC   DBCOMFCS,COMFACS                                                 
         MVC   DBFILE,=C'TP '                                                   
         CLI   OVSYS,3             CHECK FOR NETWORK                            
         BNE   *+10                                                             
         MVC   DBFILE,=C'NAD'                                                   
         MVI   DBSELMED,C'T'                                                    
         CLI   QMED,C'R'           FOR MEDIA = RADIO                            
         BNE   *+8                                                              
         MVI   DBSELMED,C'R'                                                    
         CLI   SVAPROF+7,C'C'        SEE IF CANADIAN AGY                        
         BNE   DR110                                                            
         CLI   SVCLEX,C'U'         CANADIAN - SEE IF USING US DEMOS             
         BE    DR110               YES                                          
         MVI   DBSELMED,C'C'                                                    
*                                                                               
DR110    MVC   DMCB+4(4),=X'D9000AE0'   DEMOCON                                 
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(14,EDEMLST),(13,REC2),(C'S',DBLOCK),EUSRNMS           
         DROP  R4                                                               
         LA    R5,ESTDEMS                                                       
         LA    R2,EDEMLST                                                       
         LA    R6,L'ESTDEMS(R5)                                                 
         L     RF,AIO                                                           
*                                                                               
DR120    CLI   0(RF),C' '                                                       
         BNH   DR140               LAST DEMO                                    
         BAS   RE,FMTDEMO          FORMAT DEMO - RETURNS WITH LENGTH            
*                                  WORK AND WORK+1 HAS DEMO DESC                
         ZIC   R1,WORK                                                          
         LR    R0,R5               SEE IF IT WILL FIT ON THIS LINE              
         AR    R0,R1                                                            
         CR    R0,R6                                                            
         BNH   DR130                                                            
         BCTR  R5,0                                                             
         CLI   0(R5),C','                                                       
         BNE   *+8                                                              
         MVI   0(R5),C' '          BLANK LAST COMMA                             
         LA    R5,ESTDEM2                                                       
         LA    R6,L'ESTDEM2(R5)                                                 
*                                                                               
DR130    BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),WORK+1                                                   
         AR    R5,R1                                                            
         LA    R5,1(R5)                                                         
         CR    R5,R6               SEE IF AT END OF LINE                        
         BNL   *+8                                                              
         MVI   0(R5),C','                                                       
         LA    R5,1(R5)                                                         
         LA    RF,11(RF)           NEXT DEMO IN REC2                            
         LA    R2,3(R2)            NEXT DEMO IN EDEMLST                         
         B     DR120                                                            
*                                                                               
DR140    BCTR  R5,0                                                             
         CLI   0(R5),C','          BLANK LAST COMMA                             
         BNE   DR150                                                            
         MVI   0(R5),C' '                                                       
*                                                                               
DR150    OI    ESTDEMSH+6,X'80'                                                 
         OI    ESTDEM2H+6,X'80'                                                 
*                                                                               
**********************************************************************          
*                                                                               
         XC    ESTWTS,ESTWTS                                                    
         LA    R5,ESTWTS                                                        
         LA    R6,L'ESTWTS(R5)                                                  
         CLI   EWGTNM,C' '         SEE IF I HAVE WEIGHTS                        
         BNH   DR190               GO CHECK FOR TARGETS                         
         L     RF,AIO                                                           
         LA    R3,DMAX             FOR BCT                                      
         LA    R2,EDEMLST                                                       
         LA    R4,EWGTLST                                                       
*                                                                               
DR160    CLI   0(R4),0             SEE IF THIS DEMO HAS A WEIGHT                
         BE    DR180                                                            
         BAS   RE,FMTDEMO                                                       
         CLI   1(R2),X'21'         SEE IF USER DEMO                             
         BNE   DR170                                                            
         MVI   WORK,2              SO ONLY UN=NN WILL APPEAR                    
*                                  ON WEIGHTS LINE                              
DR170    ZIC   R1,WORK             WORK HAS LENGHT                              
         LR    R0,R5               SEE IF IT WILL FIT ON THIS LINE              
         AR    R0,R1                                                            
         AH    R0,=H'4'            ADJUST FOR WEIGHT                            
         CR    R0,R6                                                            
         BNH   *+6                                                              
         DC    H'0'                TOO MANY WEIGHTS                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),WORK+1      WORK+1 HAS DESC                              
         AR    R5,R1                                                            
         MVI   1(R5),C'='                                                       
         LA    R5,2(R5)                                                         
         BAS   RE,EDITB1                                                        
         MVI   0(R5),C','                                                       
         LA    R5,1(R5)                                                         
*                                                                               
DR180    LA    RF,11(RF)           NEXT DEMO                                    
         LA    R4,1(R4)            NEXT WEIGHT                                  
         LA    R2,3(R2)            NEXT DEMO IN EDEMLST                         
         BCT   R3,DR160                                                         
*                                                                               
DR190    OC    ETRGLST,ETRGLST     CHK FOR TARGETS                              
         BZ    DR240                                                            
         LA    R1,ETRGLST                                                       
         LA    R4,ONETWO           TARGET NUMBER LIST                           
*                                                                               
DR200    OC    0(3,R1),0(R1)                                                    
         BZ    DR230               GO DO NEXT TARGET                            
         L     RF,AIO                                                           
         LA    R2,EDEMLST                                                       
         LA    R3,DMAX                                                          
*                                                                               
DR210    CLC   0(3,R1),0(R2)                                                    
         BE    DR220                                                            
         LA    RF,11(RF)                                                        
         LA    R2,3(R2)                                                         
         BCT   R3,DR210                                                         
         DC    H'0'                TARGET NOT IN EDEMLIST                       
*                                                                               
DR220    BAS   RE,FMTDEMO                                                       
         CLI   1(R2),X'21'         SEE IF USER DEMO                             
         BNE   *+8                                                              
         MVI   WORK,2              SO ONLY UN=NN WILL APPEAR                    
*                                  ON WEIGHTS LINE                              
         ZIC   RF,WORK             WORK HAS LENGTH                              
         LR    R0,R5               SEE IF IT WILL FIT ON THIS LINE              
         AR    R0,RF                                                            
         AH    R0,=H'4'            ADJUST FOR TARGET                            
         CR    R0,R6                                                            
         BNH   *+6                                                              
         DC    H'0'                TOO MANY WEIGHTS/TARGETS                     
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),WORK+1      WORK+1 HAS DESC                              
         AR    R5,RF                                                            
         MVI   1(R5),C'='                                                       
         MVI   2(R5),C'T'                                                       
         LA    R5,3(R5)                                                         
         BAS   RE,EDITB1                                                        
         MVI   0(R5),C','                                                       
         LA    R5,1(R5)                                                         
*                                                                               
DR230    LA    R1,3(R1)            NEXT TARGET                                  
         LA    R4,1(R4)            NEXT TARGET NUMBER                           
         CLI   0(R4),X'FF'         END OF LIST                                  
         BNE   DR200                                                            
*                                                                               
DR240    BCTR  R5,0                                                             
         CLI   0(R5),C','                                                       
         BNE   *+8                                                              
         MVI   0(R5),C' '          BLANK LAST COMMA                             
*                                                                               
         OI    ESTWTSH+6,X'80'                                                  
*                                                                               
**********************************************************************          
*                                                                               
         MVC   ESTCOPY,ECOPY                                                    
         OI    ESTCOPYH+6,X'80'                                                 
*                                                                               
**********************************************************************          
*                                                                               
         XC    ESTREP,ESTREP                                                    
         OC    EREP,EREP                                                        
         BZ    DR250                                                            
         MVC   HALF,EREP                                                        
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ESTREP,DUB                                                       
DR250    OI    ESTREPH+6,X'80'                                                  
*                                                                               
**********************************************************************          
*                                                                               
         OI    ESTRBKH+6,X'80'                                                  
         OC    EBOOK,EBOOK                                                      
         BNZ   DR260                                                            
         MVC   ESTRBK,=C'LATEST'                                                
         B     DR290                                                            
*                                                                               
DR260    CLI   QMED,C'N'          NETWORK FORMAT                                
         BNE   DR270                                                            
         XC    ESTRBK,ESTRBK                                                    
         ZIC   R0,EBOOK+1                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ESTRBK(2),DUB                                                    
         MVI   ESTRBK+2,C'/'                                                    
         ZIC   R0,EBOOK                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ESTRBK+3(2),DUB                                                  
         B     DR290                                                            
*                                                                               
DR270    GOTO1 DATCON,DMCB,(3,EBOOK),(6,ESTRBK)                                 
*                                                                               
**********************************************************************          
*                                                                               
DR290    OI    ESTHUTH+6,X'80'                                                  
         CLI   EHUTADJ,0                                                        
         BNE   DR300                                                            
         MVC   ESTHUT,=C'AUTO'                                                  
         B     DR310                                                            
*                                                                               
DR300    SR    R4,R4                                                            
         IC    R4,EHUTADJ                                                       
         XC    ESTHUT,ESTHUT                                                    
         SRL   R4,4                                                             
         STC   R4,WORK+1                                                        
         MVI   WORK,77                                                          
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(3,WORK),(6,WORK+10)                                 
         MVC   ESTHUT(3),WORK+10                                                
*                                                                               
**********************************************************************          
*                                                                               
DR310    MVC   ESTMENU,EDAYMENU                                                 
         OI    ESTMENUH+6,X'80'                                                 
*                                                                               
**********************************************************************          
*                                                                               
         MVC   ESTFLTR,EPROF                                                    
         OI    ESTFLTRH+6,X'80'                                                 
*                                                                               
**********************************************************************          
*                                                                               
         XC    ESTCPPE,ESTCPPE                                                  
         LA    R6,ESTCPPE                                                       
         OC    ECPPCLT,ECPPCLT                                                  
         BZ    DR320                    NO CLT                                  
         MVC   DMCB+4(4),=X'D9000A15'                                           
         GOTO1 CALLOV,DMCB,0                                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),ECPPCLT,0(R6)                                          
         LA    R6,2(R6)                                                         
         CLI   0(R6),C' '                                                       
         BE    *+8                                                              
         LA    R6,1(R6)                                                         
         MVI   0(R6),C'/'                                                       
         LA    R6,1(R6)                                                         
*                                                                               
DR320    CLI   ECPPEST,0                                                        
         BE    DR330                                                            
         ZIC   R0,ECPPEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R6),DUB                                                      
*                                                                               
DR330    OI    ESTCPPEH+6,X'80'                                                 
*                                                                               
**********************************************************************          
*                                                                               
         XC    ESTTYPE,ESTTYPE                                                  
         CLI   ETYPE,0                                                          
         BE    DR360                                                            
         MVC   ESTTYPE(3),=C'CUT'                                               
         CLI   ETYPE,C'C'                                                       
         BE    DR360                                                            
         XC    ESTTYPE,ESTTYPE                                                  
         LA    R4,FTYPTAB                                                       
*                                                                               
DR340    CLC   ETYPE,2(R4)                                                      
         BE    DR350                                                            
         LA    R4,3(R4)                                                         
         CLI   0(R4),X'FF'         END OF TABLE                                 
         BNE   DR340                                                            
         B     DR360                                                            
*                                                                               
DR350    MVC   ESTTYPE(2),0(R4)                                                 
*                                                                               
DR360    OI    ESTTYPEH+6,X'80'                                                 
*                                                                               
**********************************************************************          
*                                                                               
         XC    ESTRNGE,ESTRNGE                                                  
         OC    EREQLO(2),EREQLO                                                 
         BZ    DR370                                                            
         MVC   ESTRNGE(2),=C'NO'                                                
         CLC   EREQLO(2),=C'NO'                                                 
         BE    DR370                                                            
         ZIC   R0,EREQLO                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ESTRNGE(3),DUB                                                   
         MVI   ESTRNGE+3,C'-'                                                   
         ZIC   R0,EREQHI                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ESTRNGE+4(3),DUB                                                 
*                                                                               
DR370    OI    ESTRNGEH+6,X'80'                                                 
*                                                                               
**********************************************************************          
*                                                                               
         XC    ESTRTYP,ESTRTYP                                                  
         MVC   ESTRTYP(1),ERATE                                                 
         MVC   ESTRTYP+1(1),ERATECST                                            
         OI    ESTRTYPH+6,X'80'                                                 
*                                                                               
**********************************************************************          
*                                                                               
         MVC   ESTOWD(4),=C'NO  '                                               
         CLI   EOWSDAY,0                                                        
         BE    *+10                                                             
         MVC   ESTOWD(4),=C'YES '                                               
         OI    ESTOWDH+6,X'80'                                                  
*                                                                               
**********************************************************************          
*                                                                               
*                                **CONTROL FIELD - SEE NOTE TOP OF PR           
         XC    ESTECON,ESTECON                                                  
         TM    ECONTROL,EBILESTQ                                                
         BNO   *+8                                                              
         MVI   ESTECON,C'E'                                                     
         TM    ECONTROL,ENSEPCMQ                                                
         BNO   *+10                                                             
         MVC   ESTECON(3),=C'NSC'                                               
         OI    ESTECONH+6,X'80'                                                 
*                                                                               
**********************************************************************          
*                                                                               
         MVC   ESTERTL,ERTLSCHM                                                 
         OI    ESTERTLH+6,X'80'                                                 
*                                                                               
**********************************************************************          
*                                                                               
         XC    ESTUSR1,ESTUSR1                                                  
         OC    EUSER1,EUSER1                                                    
         BZ    *+10                                                             
         MVC   ESTUSR1,EUSER1                                                   
         OI    ESTUSR1H+6,X'80'                                                 
*                                                                               
         XC    ESTUSR2,ESTUSR2                                                  
         OC    EUSER2,EUSER2                                                    
         BZ    *+10                                                             
         MVC   ESTUSR2(16),EUSER2                                               
         OI    ESTUSR2H+6,X'80'                                                 
*                                                                               
**********************************************************************          
*                                                                               
         XC    SCRNFLAG,SCRNFLAG   INITIALIZE THE FLAG                          
         LA    R1,ESTOPT                                                        
         MVC   0(L'ESTOPT,R1),SPACES                                            
         CLI   SVCLDLY,C'Y'        SEE IF CLT DEFAULT SET                       
         BE    DR380                                                            
         CLI   EDAILY,C' '                                                      
         BNH   DR390                                                            
*                                                                               
DR380    LA    R0,8                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   DR390               NO - SO CONTINUE                             
*                                                                               
         CLI   EDAILY,C' '         MAKE SURE TO DISPLAY SOMETHING               
         BH    *+8                                                              
         MVI   EDAILY,C'N'                                                      
         MVC   0(6,R1),=C'DAILY='                                               
         MVC   6(1,R1),EDAILY                                                   
         MVI   7(R1),C','                                                       
         LA    R1,8(R1)                                                         
*                                                                               
DR390    TM    EFLAG1,EF1REQ       IS THIS REQUESTABLE                          
         BNO   DR400                                                            
*                                                                               
         LA    R0,6                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   DR400               NO - SO CONTINUE                             
*                                                                               
         MVC   0(6,R1),=C'REQ=Y,'                                               
         LA    R1,6(R1)                                                         
*                                                                               
DR400    DS    0H                                                               
         TM    EFLAG1,EF1NMG       NEW MAKEGOODS                                
         BNO   DR410                                                            
*                                                                               
         LA    R0,6                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   DR410               NO - SO CONTINUE                             
*                                                                               
         MVC   0(6,R1),=C'NMG=Y,'                                               
         LA    R1,6(R1)                                                         
*                                                                               
DR410    TM    EFLAG1,EF1NODEM     NO DEMOS REQUIRED FOR BUY                    
         BNO   DR420                                                            
*                                                                               
         LA    R0,8                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   DR420               NO - SO CONTINUE                             
*                                                                               
         MVC   0(8,R1),=C'DEMOS=N,'                                             
         LA    R1,8(R1)                                                         
*                                                                               
DR420    DS    0H                                                               
         OC    ECGTPCT,ECGTPCT     CLIENT GROSS TRADE                           
         BZ    DR430                                                            
*                                                                               
         LA    R0,10               LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   DR430               NO - SO CONTINUE                             
*                                                                               
         MVC   0(4,R1),=C'CGT='                                                 
         LA    R1,4(R1)                                                         
         LR    R3,R1                                                            
         EDIT  ECGTPCT,(5,(R3)),2,ALIGN=LEFT                                    
         LR    R1,R3                                                            
         AR    R1,R0                                                            
         MVI   0(R1),C','                                                       
         LA    R1,1(R1)                                                         
*                                                                               
DR430    TM    EFLAG1,EF1OOWPW                                                  
         BZ    DR440                                                            
*                                                                               
         LA    R0,5                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   DR440               NO - SO CONTINUE                             
*                                                                               
         MVC   0(4,R1),=C'OWPW'                                                 
         MVI   4(R1),C','                                                       
         LA    R1,5(R1)                                                         
*                                                                               
DR440    OC    ECOST2,ECOST2       ANY COST FACTOR?                             
         BZ    DR470                                                            
*                                                                               
         LA    R0,13               MAX LENGTH OF OUTPUT DATA                    
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   DR470               NO - SO CONTINUE                             
*                                                                               
         MVC   0(5,R1),=C'COS2='                                                
         LA    R4,5(R1)                                                         
         CLI   ECOST2,X'80'        ZERO AS INPUT DATA?                          
         BNE   DR450               NO - SO CONTINUE                             
*                                                                               
         MVC   0(3,R4),=C'0.0'     ELSE - MOVE OUT ZERO                         
         LA    R0,3                INC A(LINE POSITION)                         
         B     DR460               AND CONTINUE                                 
*                                                                               
DR450    EDIT  ECOST2,(8,0(R4)),6,ALIGN=LEFT,FILL=0,DROP=5                      
*                                                                               
DR460    AR    R4,R0                                                            
         MVI   0(R4),C','                                                       
         LA    R1,1(R4)                                                         
*                                                                               
DR470    CLI   ESLN,0              RESTRICTED SPOT LEN?                         
         BE    DR480                                                            
*                                                                               
         LA    R0,7                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   DR480               NO - SO CONTINUE                             
*                                                                               
         MVC   0(4,R1),=C'SLN='                                                 
         LA    R1,4(R1)                                                         
         LR    R3,R1                                                            
         EDIT  ESLN,(3,(R3)),0,ALIGN=LEFT                                       
         AR    R3,R0                                                            
         MVI   0(R3),C','                                                       
         LA    R1,1(R3)                                                         
*                                                                               
DR480    CLI   ECASHPRD,0          CASH PRD?                                    
         BE    DR510                                                            
*                                                                               
         LA    R0,8                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   DR510               NO - SO CONTINUE                             
*                                                                               
         MVC   0(5,R1),=C'CASH='                                                
         LA    R1,5(R1)                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(4),ESTKEY                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     RF,AIO                                                           
         USING CLTHDR,RF                                                        
         LA    RF,CLIST-CLTHDR(RF) FIND CASH PRD                                
DR490    CLI   0(RF),0                                                          
         BE    DR510                                                            
         CLC   3(1,RF),ECASHPRD    MATCH ON CASH PRD NUMBER                     
         BE    DR500                                                            
         LA    RF,4(RF)                                                         
         B     DR490                                                            
DR500    MVC   0(3,R1),0(RF)                                                    
         LA    R1,3(R1)                                                         
         DROP  RF                                                               
*                                                                               
         MVI   0(R1),C','                                                       
         LA    R1,1(R1)                                                         
*                                                                               
DR510    CLI   ETRDPRD,0           TRADE PRODUCT?                               
         BE    DR540                                                            
*                                                                               
         LA    R0,7                LENGTH OF OUTPUT DATA                        
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   DR540               NO - SO CONTINUE                             
*                                                                               
         MVC   0(4,R1),=C'TRD='                                                 
         LA    R1,4(R1)                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(4),ESTKEY                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     RF,AIO                                                           
         LA    RF,CLIST-CLTHDR(RF) FIND CASH PRD                                
DR520    CLI   0(RF),0                                                          
         BE    DR540                                                            
         CLC   3(1,RF),ETRDPRD     MATCH ON TRADE PRD NUMBER                    
         BE    DR530                                                            
         LA    RF,4(RF)                                                         
         B     DR520                                                            
DR530    MVC   0(3,R1),0(RF)                                                    
         LA    R1,3(R1)                                                         
*                                                                               
         MVI   0(R1),C','                                                       
         LA    R1,1(R1)                                                         
*                                                                               
DR540    BCTR  R1,0                                                             
         CLI   0(R1),C','                                                       
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
*                                                                               
         OC    EPWPCT,EPWPCT       NO PW % TO PRINT                             
         BZ    DR590                                                            
*                                                                               
         LA    R0,11               MAX LENGTH OF OUTPUT DATA                    
         BAS   RE,CHECKOPT         WILL IT FIT IN FIELD?                        
         BNZ   DR590               NO - SO CONTINUE                             
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,14,EPWPCT                                                     
         SRA   R3,8                                                             
         LA    R2,ESTOPT              R2-->1ST BYTE                             
         LA    R4,(L'ESTOPT-1)(R2)    R4-->LAST BYTE                            
DR550    CR    R2,R4               FIND LAST NON-BLANK                          
         BNL   DR570                                                            
         CLI   0(R4),C' '                                                       
         BNE   DR560                                                            
         BCTR  R4,0                                                             
         B     DR550                                                            
*                                                                               
DR560    MVI   1(R4),C','                                                       
         LA    R4,2(R4)                                                         
DR570    MVC   0(3,R4),=C'PW='                                                  
         CLC   EPWPCT,=X'800000'      REALLY 0                                  
         BNE   DR580                                                            
         MVI   3(R4),C'0'                                                       
         B     DR590                                                            
*                                                                               
DR580    EDIT  (R3),(7,3(R4)),2,ALIGN=LEFT,TRAIL=0,FLOAT=-                      
*                                                                               
DR590    CLI   SCRNFLAG,0          DID EVERYTHING FIT?                          
         BE    DR600               YES - SO CONTINUE                            
*                                                                               
         MVC   0(2,R1),=C',*'      ELSE - MOVE OUT 'DIDN'T FIT' FLAG            
*                                                                               
DR600    OI    ESTOPTH+6,X'80'                                                  
*                                                                               
DRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       DISPLAY KEY                                   *         
***********************************************************************         
*                                                                               
DK       L     R3,AIO                                                           
         USING EKEY,R3                                                          
         MVC   BYTE,EKEYAM           ISOLATE MEDIA CODE                         
         NI    BYTE,X'0F'                                                       
         LA    R5,MEDTAB             FIND MEDIA CODE USING MEDIA TABLE          
DK10     CLC   BYTE,1(R5)                                                       
         BE    DK20                                                             
         LA    R5,MEDTABLQ(R5)                                                  
         CLI   0(R5),X'FF'                                                      
         BNE   DK10                                                             
DK20     MVC   ESTMEDK,0(R5)                                                    
         OI    ESTMEDKH+6,X'80'                                                 
         MVI   ESTMEDKH+5,1          TRANSMIT MEDIA CODE TO SCREEN              
*                                                                               
***********************************************************************         
*                                                                               
         GOTO1 CLUNPK,DMCB,EKEYCLT,ESTCLIK                                      
         OI    ESTCLIKH+6,X'80'                                                 
         MVI   ESTCLIKH+5,3          TRANSMIT CLIENT CODE TO SCREEN             
*                                                                               
***********************************************************************         
*                                                                               
         MVC   ESTPROK,EKEYPRD                                                  
         MVI   ESTPROKH+5,3                                                     
         OI    ESTPROKH+6,X'80'      TRANSMIT PRODUCT CODE TO SCREEN            
*                                                                               
**********************************************************************          
*                                                                               
         GOTO1 HEXOUT,DMCB,EKEYEST,ESTESTK,L'EKEYEST                            
         MVI   ESTESTKH+5,2          LENGTH                                     
         OI    ESTESTKH+6,X'80'      TRANSMIT ESTIMATE CODE TO SCREEN           
*                                                                               
DKX      B     VK                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        DELETE PRODUCT RECORDS                                       *         
***********************************************************************         
*                                                                               
DEL      MVC   KEY,PROKEY            READ CLIENT RECORD INTO AIO2               
         XC    KEY+4(3),KEY+4                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         MVC   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R5,AIO2                                                          
         USING CLTHDR,R5                                                        
*                                                                               
***********************************************************************         
*                                                                               
         LA    R2,PRDPRONH                                                      
         CLC   QPRD,=C'POL'                                                     
         BNE   DEL10                                                            
*                                                                               
         MVC   ERRNUM,=AL2(DELERR8)  POL PRODUCT CAN ONLY BE DELETED            
         CLC   CLIST(3),=C'POL'      IF IT'S THE ONLY PRODUCT IN THE            
         BE    DEL10                 CLIENT'S CLIST                             
         B     SPERREX                                                          
*                                                                               
***********************************************************************         
*                                                                               
DEL10    MVC   ERRNUM,=AL2(DELERR9)  USER MUST INPUT 'DELETE' IN                
         CLC   8(6,R2),=C'DELETE'    PRODUCT NAME FIELD                         
         BNE   SPERREX                                                          
*                                                                               
***********************************************************************         
*                                                                               
         USING PRDHDR,R6                                                        
         MVC   AIO,AIO1              PRODUCT CANNOT BE DELETED IF IT'S          
         L     R6,AIO                IN A PRODUCT GROUP                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D81'       RECORD TYPE PRODUCT GROUP                  
         MVC   KEY+2(1),BAGYMD       AGENCY/MEDIA ...                           
         MVC   KEY+3(2),BCLT         CLIENT                                     
         GOTO1 HIGH                                                             
*                                                                               
DEL20    CLC   KEY(5),KEYSAVE                                                   
         BNE   DEL30                                                            
         CLC   KEY+8(3),QPRD         PRODUCT GROUP W/ SAME BASE KEY             
         BNE   DEL25                 (A/M,CLT) FOUND, IF PRODUCT CODE           
         MVC   ERRNUM,=AL2(DELERR1)  ALSO MATCHES THEN PRODUCT CANNOT           
         MVC   CONHEADH+32,KEY+5     BE DELETED                                 
         B     SPERREX                                                          
DEL25    GOTO1 SEQ                   LOOP THROUGH ALL PRODUCTS FOR              
         B     DEL20                 PRODUCT GROUP                              
*                                                                               
***********************************************************************         
*                                                                               
DEL30    LA    R2,PRDCLINH                                                      
         MVC   ERRNUM,=AL2(DELERR2)  DELETE NOT ALLOWED IF CLIENT HAS           
         OC    CMCLTCOD,CMCLTCOD     MASTER TRAFFIC REFERENCE                   
         BZ    DEL35                                                            
         B     SPERREX                                                          
*                                                                               
***********************************************************************         
*                                                                               
DEL35    LA    R2,PRDPRONH                                                      
         MVC   KEY,PROKEY            MOVE X'01' TO THE END OF PROKEY            
         MVI   KEY+7,X'01'           TO READ FIRST ESTIMATE OR BILL             
         GOTO1 HIGH                  RECORD                                     
         B     DEL50                                                            
*                                                                               
DEL40    GOTO1 SEQ                   READ EST. OR BILL RECORD AFTER             
*                                    THE FIRST                                  
*                                                                               
DEL50    CLC   KEYSAVE(7),KEY        IF NO MORE ESTIMATE OR BILL                
         BNE   DEL90                 RECORDS FOUND BRANCH TO DEL110             
         CLI   KEY+8,0               IF BILL RECORD BRANCH TO DEL90             
         BNE   DEL90                                                            
*                                                                               
***********************************************************************         
***********************************************************************         
*                                                                               
DEL90    MVC   ERRNUM,=AL2(DELERR5)  BILL RECORD EXISTS ... PRODUCT             
         B     SPERREX               CANNOT BE DELETED                          
*                                                                               
***********************************************************************         
*                                                                               
***********************************************************************         
*                                                                               
DEL220   MVC   KEY,PROKEY            VALIDATION COMPLETE - OK TO DELETE         
DEL230   GOTO1 HIGH                  PRODUCT - READ IT BACK IN                  
         B     DEL250                                                           
*                                                                               
DEL240   GOTO1 SEQ                   READ PRODUCT'S ESTIMATE RECORDS            
DEL250   CLC   KEYSAVE(7),KEY                                                   
         BNE   DEL260                                                           
*                                                                               
         OI    KEY+13,X'80'                                                     
         GOTO1 WRITE                                                            
*                                                                               
         MVC   AIO,AIO3              GET PRODUCT OR ESTIMATE RECORD             
         GOTO1 GETREC                                                           
         L     RE,AIO                                                           
         OI    15(RE),X'80'          MARK RECORD DELETED                        
         BAS   RE,PTREC                                                         
         B     DEL240                                                           
*                                                                               
***********************************************************************         
*                                                                               
DEL260   CLI   SVAPROF+7,C'C'        IF CANADIAN AGENCY, MEDIA TV ...           
         BNE   DELX                                                             
         CLI   QMED,C'T'                                                        
         BNE   DELX                                                             
*                                                                               
         MVC   WORK(1),KEYSAVE+1                                                
         NI    WORK,X'0F'                                                       
         CLI   WORK,X'08'                                                       
         BE    DELX                                                             
         CLI   WORK,X'03'                                                       
         BNE   DEL270                                                           
*                                                                               
         MVC   KEY,PROKEY            MARK PRODUCT RECORD FOR MEDIA              
         NI    KEY+1,X'F0'           C(08) AND IT'S ESTIMATE RECORDS            
         OI    KEY+1,X'08'           DELETED                                    
         B     DEL230                                                           
*                                                                               
DEL270   MVC   KEY,PROKEY            MARK PRODUCT RECORD FOR MEDIA              
         NI    KEY+1,X'F0'           N(03) AND IT'S ESTIMATE RECORDS            
         OI    KEY+1,X'03'           DELETED                                    
         B     DEL230                                                           
*                                                                               
***********************************************************************         
*                                                                               
DELX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        SETUP                                                        *         
***********************************************************************         
SETUP    NTR1                                                                   
         MVI   USEIO,C'N'                                                       
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    GENSTAT4,CONFDEL                                                 
         OI    CONSERVH+1,X'01'      MODIFY SERVICE REQUEST                     
         OI    CONSERVH+6,X'80'      TRANSMIT TO GET CONTROL                    
         MVI   IOOPT,C'Y'                                                       
*                                                                               
         OI    PRDREH+1,X'0C'        HIDE PF12=RETURN FIELD                     
         LR    RE,RA                                                            
         AH    RE,=Y(TWAENDLQ-2)                                                
         CLI   1(RE),0                                                          
         BE    *+8                                                              
         NI    PRDREH+1,X'FF'-X'04'  LIGHT UP PF12 FIELD                        
         OI    PRDREH+6,X'80'                                                   
*                                                                               
SETUP10  GOTO1 INITPFKY,DMCB,PFTABLE PF TABLE                                   
*                                                                               
SETUPX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       PUT RECORD                                    *         
***********************************************************************         
*                                                                               
PTREC    NTR1                                                                   
         MVI   USEIO,C'Y'                                                       
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTFIL',KEY+14,AIO                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   USEIO,C'N'                                                       
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                       ADD RECORD                                    *         
***********************************************************************         
*                                                                               
ADREC    NTR1                                                                   
         MVI   USEIO,C'Y'                                                       
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTFIL',KEY+14,AIO                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   USEIO,C'N'                                                       
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        VALPST                                                       *         
***********************************************************************         
VALPST   NTR1                                                                   
         LA    R4,ELEM                                                          
         USING PSTBLKD,R4            SETUP PARAMETERS FOR PSTVAL CALL           
         XC    ELEM,ELEM             (VALIDATE PST CODES)                       
         XC    PSTOUT,PSTOUT                                                    
         MVI   PSTACT,PSTVALQ        ACTION = VALIDIATE                         
         LA    R1,PRDPSTH                                                       
         ST    R1,PSTADIN            INPUT = PST SCREEN FIELD                   
         LA    R1,PSTOUT                                                        
         ST    R1,PSTADOUT           OUTPUT = PSTADOUT                          
         MVC   PSTACOM,ACOMFACS                                                 
         MVI   DMCB+7,QPSTVAL                                                   
         XC    DMCB(7),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB               VALID PST CODES RETURNED IN PPST           
         GOTO1 (RF),DMCB,(R4)                                                   
*                                                                               
         MVC   ERRNUM,=AL2(PSTERR1)                                             
         CLI   PSTERR,0                                                         
         BNE   SPERREX                                                          
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        DISPPST                                                      *         
***********************************************************************         
DISPPST  NTR1                                                                   
         MVC   PRDPST,SPACES         CLEAR PST CODE SCREEN FIELD                
         OI    PRDPSTH+6,X'80'                                                  
*                                                                               
         OC    SVPPST,SVPPST         ANY PST CODE TO DISPLAY?                   
         BZ    DPX                   IF NO, SEND BLANK FIELD TO SCREEN          
*                                                                               
         LA    R4,ELEM               IF YES, USE PROVINCIAL TAX VALID-          
         USING PSTBLKD,R4            ATION BLOCK - SET UP PARAMETERS            
         XC    ELEM,ELEM             FOR CALL TO PSTVAL                         
         XC    PSTOUT,PSTOUT                                                    
         MVI   PSTACT,PSTFMTQ        ACTION = FORMAT                            
         LA    R1,SVPPST                                                        
         ST    R1,PSTADIN            INPUT = SVPPST FIELD OF PRO.RECORD         
         LA    R1,PSTOUT                                                        
         ST    R1,PSTADOUT           OUTPUT = PSTADOUT                          
         MVI   DMCB+7,QPSTVAL                                                   
         XC    DMCB(7),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(R4)                                                   
*                                                                               
         MVC   PRDPST,PSTOUT         COPY VALIDATED PST CODES TO SCREEN         
         OI    PRDPSTH+6,X'80'       TRANSMIT                                   
         DROP  R4                                                               
DPX      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        EDTUSR                                                       *         
***********************************************************************         
EDTUSR   NTR1                                                                   
         L     R3,AUSR               ANY INPUT IN USER FIELD?                   
         CLI   5(R3),0                                                          
         BNE   EDTUSR10                                                         
*                                                                               
         TM    FLAG1,CFLGREQQ        NO INPUT ... WAS IT REQUIRED?              
         BZ    XIT                   IF NOT, EXIT                               
         B     ERRMIS                IF YES, ERROR                              
*                                                                               
EDTUSR10 MVC   ERRNUM,=AL2(TOOLONG)                                             
         CLC   LEN,5(R3)             CHECK LENGTH OF INPUT                      
         BL    SPERREX                                                          
*                                                                               
         CLI   UTYPE,C' '            IS TYPE SUPPOSED TO BE 'WILD'?             
         BNH   EDTUSR80                                                         
*                                                                               
*                                                                               
         CLI   UTYPE,C'C'            IF TYPE IS CHARACTER...                    
         BNE   EDTUSR60              INPUT CANNOT BE NUMERIC                    
         LA    R4,8(R3)                                                         
         ZIC   R1,5(R3)                                                         
EDTUSR40 MVC   ERRNUM,=AL2(EDTERR1)                                             
         CLI   0(R4),C'0'                                                       
         BL    EDTUSR50                                                         
         CLI   0(R4),C'9'                                                       
         BNH   SPERREX                                                          
EDTUSR50 LA    R4,1(R4)              CHECK NEXT CHAR IN INPUT                   
         BCT   R1,EDTUSR40                                                      
         B     EDTUSR80                                                         
*                                                                               
*                                                                               
EDTUSR60 CLI   UTYPE,C'N'            IF TYPE IS NUMERIC...                      
         BNE   EDTUSR70                                                         
         BAS   RE,CHKNTYP            INPUT MUST BE ALL NUMERIC                  
         BE    EDTUSR80                                                         
         MVC   ERRNUM,=AL2(EDTERR2)                                             
         B     SPERREX                                                          
*                                                                               
*                                                                               
EDTUSR70 MVC   ERRNUM,=AL2(BADDATE)                                             
         CLI   UTYPE,C'D'            IS TYPE DATE...                            
         BE    *+6                                                              
         DC    H'0'                  IF NO, BAD TYPE                            
         GOTO1 DATVAL,DMCB,(0,8(R3)),WORK                                       
         OC    DMCB(4),DMCB          INPUT MUST BE VALID DATE                   
         BZ    SPERREX                                                          
         L     R1,0(R1)                                                         
         ZIC   R4,5(R3)                                                         
         SR    R1,R4                                                            
         BNZ   SPERREX                                                          
*                                                                               
EDTUSR80 ZIC   R1,5(R3)                                                         
         BCTR  R1,0                                                             
         EXMVC R1,USERDATA,8(R3)     MOVE INPUT INTO USERDATA                   
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        CHKNTYP                                                      *         
***********************************************************************         
* THIS ROUTINE CHECKS TO SEE IF PASSED VARIABLE IS VALID NUMERIC                
***********************************************************************         
CHKNTYP  NTR1                                                                   
         LA    R4,8(R3)              R4 ---> INPUT                              
         ZIC   R1,5(R3)              R1= L(INPUT)                               
CHKN10   LA    R5,VALDNTBL           R5 = TABLE OF VALID DIGITS                 
CHKN20   CLC   0(1,R4),0(R5)         VALID DIGIT?                               
         BE    CHKN30                                                           
         LA    R5,1(R5)              BUMP TO NEXT DIGIT                         
         CLC   0(1,R5),EOTBLQ        END OF TABLE?                              
         BE    XCHKN                                                            
         B     CHKN20                NO, TRY AGAIN                              
*                                                                               
CHKN30   LA    R4,1(R4)              CHECK NEXT CHAR IN INPUT                   
         BCT   R1,CHKN10                                                        
XCHKN    LTR   R1,R1                                                            
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        TESTTRAD                                                     *         
***********************************************************************         
* THIS ROUTINE CHECKS IF CLIENT IS TRADE AND IF SO ADDS THE 'TRADE'             
* TYPE PRODUCT CODE AND NUMBER TO THE CLIENT RECORD TABLE.                      
***********************************************************************         
TESTTRAD NTR1                                                                   
         USING PRDHDR,R6                                                        
         L     R6,AIO1                                                          
         TM    SVAGYFL1,X'02'        CLIENT MUST BE EITHER A TRADE              
         BO    TTRD10                AGENCY OR TRADE CLIENT OR SUB-             
         TM    SVCLOP2,COP2TRAD      ROUTINE EXITS                              
         BZ    TTRDX                                                            
TTRD10   CLI   QPRD+2,C'C'           AND MUST BE A CASH PRODUCT                 
         BE    TTRD20                                                           
         MVC   ERRNUM,=AL2(ERRTRAD2)                                            
         B     SPERREX                                                          
*                                                                               
TTRD20   MVC   ERRNUM,=AL2(CLTFULL)  TOO MANY PRODUCTS ERROR CODE               
         CLI   PCODE+1,104           MORE THAN 104 PRODUCTS?                    
         BH    SPERREX               YES - SO ERROR                             
*                                                                               
         L     R5,AIO2                                                          
         USING CLTHDR,R5                                                        
         L     R4,WORK               NO - STORE OLD PRD CODE AND #              
         MVI   WORK+2,C'#'           SET THE TRADE PROD CODE                    
         OI    WORK+3,X'80'          TURN ON THE TRADE BIT IN PRD #             
         ZIC   R3,CCOUNT                                                        
         GOTO1 BINSRCH,DMCB,(X'01',WORK),CLIST,(R3),4,(0,3),218                 
*                                                                               
         MVC   CCOUNT,DMCB+11        NEW COUNT OF PRODUCTS IN TABLE             
         CLI   DMCB,X'01'            WAS PRODUCT CODE ALREADY THERE?            
         BE    TTRD30                                                           
         MVC   ERRNUM,=AL2(ERRTRADE) IF YES - RETURN ERROR                      
         B     SPERREX                                                          
*                                                                               
TTRD30   ST    R4,WORK               RESTORE CASH PRODUCT CODE/#                
         XR    R0,R0                 SET GOOD CC                                
TTRDX    XIT1                                                                   
         DROP  R5                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        CANTV PRODUCT                                                *         
***********************************************************************         
*        IF CANADIAN AGENCY ADDS OR MODIFIES A PRODUCT FOR TV - MUST  *         
*        ADD OR MODIFY PRODUCT RECORD FOR MEDIA N(03) AND MEDIA C(08) *         
***********************************************************************         
CANTV    NTR1                                                                   
         USING PRDHDR,R4                                                        
         L     R4,AIO1                                                          
         CLI   SVAPROF+7,C'C'        CANADIAN?                                  
         BNE   CTX                                                              
         CLI   QMED,C'T'             TV?                                        
         BNE   CTX                                                              
*                                                                               
         XC    KEY,KEY               MEDIA N(03)                                
         MVC   KEY,PROKEY                                                       
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'03'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    CT10                                                             
         L     R5,AIO1                                                          
         MVC   0(13,R5),KEYSAVE      RECORD DOESN'T EXIST                       
         MVC   KEY(13),KEYSAVE       MUST ADD IT                                
         MVC   AIO,AIO1                                                         
         BAS   RE,ADREC                                                         
         B     CT20                                                             
*                                                                               
CT10     MVC   AIO,AIO3              RECORD EXISTS, GET IT                      
         GOTO1 GETREC                INTO AIO3                                  
         L     R5,AIO1                                                          
         MVC   0(13,R5),KEY          COPY KEY                                   
         MVC   AIO,AIO1                                                         
         BAS   RE,PTREC              PUT N(03) RECORD                           
*                                                                               
*                                                                               
*                                                                               
CT20     XC    KEY,KEY               MEDIA C(08)                                
         MVC   KEY,PROKEY                                                       
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'08'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    CT30                                                             
         L     R5,AIO1               RECORD DOESN'T EXIST                       
         MVC   0(13,R5),KEYSAVE      MUST ADD IT                                
         MVC   AIO,AIO1                                                         
         MVC   KEY(13),KEYSAVE                                                  
         BAS   RE,ADREC                                                         
         B     CT40                                                             
*                                                                               
CT30     MVC   AIO,AIO3              RECORD EXISTS, GET IT                      
         GOTO1 GETREC                INTO AIO3                                  
         L     R5,AIO1                                                          
         MVC   0(13,R5),KEY          COPY KEY                                   
         MVC   AIO,AIO1                                                         
         BAS   RE,PTREC              PUT C(08) RECORD                           
CT40     MVC   KEY,PROKEY            RESTORE KEY                                
         NI    1(R5),X'F0'                                                      
         OI    1(R5),X'01'                                                      
CTX      XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        CANTVC CLIENT                                                *         
***********************************************************************         
*        IF CANADIAN AGENCY ADDS A PRODUCT FOR TV ADD CLIENT RECORD   *         
*        FOR MEDIA N(03) AND MEDIA C(08)                              *         
***********************************************************************         
CANTVC   NTR1                                                                   
         CLI   SVAPROF+7,C'C'        CANADIAN?                                  
         BNE   CTCX                                                             
         CLI   QMED,C'T'             TV?                                        
         BNE   CTCX                                                             
*                                                                               
         XC    KEY,KEY               SET UP CLIENT KEY FOR MEDIA N(03)          
         MVC   KEY(4),PROKEY                                                    
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'03'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    CTC10                                                            
         DC    H'0'                                                             
*                                                                               
CTC10    MVC   AIO,AIO3              READ CLIENT INTO AIO3                      
         GOTO1 GETREC                                                           
         L     R5,AIO2                                                          
         MVC   0(13,R5),KEY          COPY N(03) KEY OVER CLIENT RECORD          
         MVC   AIO,AIO2              AND PUT                                    
         BAS   RE,PTREC                                                         
*                                                                               
*                                                                               
*                                                                               
CTC20    XC    KEY,KEY               SET UP CLIENT KEY FOR MEDIA C(08)          
         MVC   KEY(4),PROKEY                                                    
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'08'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    CTC30                                                            
         DC    H'0'                                                             
*                                                                               
CTC30    MVC   AIO,AIO3              READ IT INTO AIO3                          
         GOTO1 GETREC                                                           
         L     R5,AIO2                                                          
         MVC   0(13,R5),KEY          COPY C(08) KEY OVER CLIENT RECORD          
         MVC   AIO,AIO2              AND PUT                                    
         BAS   RE,PTREC                                                         
*                                                                               
CTC40    MVC   KEY,PROKEY            RESTORE KEY                                
         NI    1(R5),X'F0'                                                      
         OI    1(R5),X'01'                                                      
CTCX     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        GENREQ                                                       *         
***********************************************************************         
GENREQ   NTR1                                                                   
         MVC   AIO,AIO3               BUILD REQUEST RECORD AT AIO3              
         L     R1,AIO                                                           
         XC    0(150,R1),0(R1)                                                  
         MVI   10(R1),41                                                        
         MVI   14(R1),106                                                       
         LA    R1,26(R1)                                                        
         MVI   0(R1),X'40'                                                      
         MVC   1(79,R1),0(R1)                                                   
         MVC   0(2,R1),=C'41'                                                   
         MVC   2(2,R1),14(RA)                                                   
         MVC   4(1,R1),QMED                                                     
         MVC   5(3,R1),QCLT                                                     
         MVC   11(3,R1),QPRD                                                    
         MVC   68(7,R1),=C'CONTROL'                                             
         MVI   61(R1),C'P'                                                      
         MVI   63(R1),C'A'                                                      
*                                                                               
         CLI   ACTEQU,ACTADD         IF ACTION = ADD, GENERATE                  
         BE    *+8                   REQUEST RECORD                             
         MVI   63(R1),C'C'                                                      
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',AIO,AIO                       
         MVC   AIO,AIO1                                                         
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        FMTDEMO                                                      *         
***********************************************************************         
*                                                                               
FMTDEMO  NTR1                      ROUTINE TO FORMAT DEMOS                      
*                                  RF POINTS TO 10 CHAR DESC                    
*                                  R2 POINTS TO 3 BYTE DEMO                     
*                                  WORK(1) RETURNS LENGTH                       
*                                  WORK+1 RETURNS DESC                          
         MVC   WORK(11),SPACES                                                  
         MVI   WORK,0                                                           
         CLI   0(RF),C' '                                                       
         BNH   FMTDEMOX                                                         
         LA    R1,11                                                            
         LA    R4,10(RF)          SCAN BACKWARDS FOR NON-SPACE                  
*                                                                               
FMTD5    CLI   0(R4),C' '                                                       
         BH    FMTD10                                                           
         BCTR  R4,0                                                             
         BCT   R1,FMTD5                                                         
*                                                                               
FMTD10   STC   R1,WORK                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+1(0),0(RF)                                                  
         CLI   1(R2),X'21'         SEE IF DOING A USER DEMO                     
         BNE   FMTD20                                                           
*                                                                               
FMTD15   MVC   WORK+11(7),WORK+1                                                
         MVC   WORK+1(3),=C'U /'                                                
*                                                                               
FMTD14   MVC   WORK+4(7),WORK+11                                                
         ZIC   R0,2(R2)            USER NAME NUMBER                             
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+2(1),DUB+7(1)                                               
         IC    R1,WORK                                                          
         AH    R1,=H'3'                                                         
         STC   R1,WORK                                                          
         B     FMTDEMOX                                                         
*                                                                               
FMTD20   CLC   WORK+1(7),EWGTNM    SEE IF IT MATCHES WEIGHTED DEMO              
         BNE   FMTDEMOX                                                         
         MVC   WORK+10(7),WORK+1                                                
         MVC   WORK+1(2),=C'W/'                                                 
         MVC   WORK+3(7),WORK+10                                                
         IC    R1,WORK                                                          
         AH    R1,=H'2'                                                         
         STC   R1,WORK                                                          
*                                                                               
FMTDEMOX XIT1                                                                   
***********************************************************************         
*                          EDITB1                                     *         
***********************************************************************         
EDITB1   NTR1                                                                   
         EDIT  (B1,0(R4)),(3,WORK2),0,ALIGN=LEFT                                
         LR    RE,R0                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),WORK2                                                    
         AR    R5,R0                                                            
         XIT1                                                                   
         SPACE                                                                  
*                                                                               
***********************************************************************         
*                                                                               
CHECKOPT NTR1                                                                   
         AR    R0,R1               A(NEW FIELD POSITION)                        
         LA    R3,ESTOPT           A(SCREEN FIELD)                              
         LA    R3,L'ESTOPT-1(R3)   A(LAST POSTITION IN FIELD)                   
         CR    R0,R3               WILL CURRENT OPTION FIT IN FIELD?            
         BH    CKOPERR             NO - SO ERROR                                
*                                                                               
         XR    R0,R0               SET GOOD CC                                  
*                                                                               
CKOPERR  ZIC   R3,SCRNFLAG         SAVE OLD COUNT OF MISSED OPTS                
         LA    R3,1(R3)            INC COUNT                                    
         STC   R3,SCRNFLAG         STORE IT                                     
         LTR   RC,RC               SET ERROR CC                                 
CKOPEXIT XIT1                                                                   
***********************************************************************         
*                          CHKSTAT                                    *         
***********************************************************************         
*                                                                               
CHKSTAT  NTR1                                                                   
         MVI   ERRCD,INVERR                                                     
         MVI   WORK,0                                                           
         CLI   5(R2),0             NO INPUT                                     
         BNE   CHKS05                                                           
         OC    ELOCKYM,ELOCKYM     NO LOCK DATES                                
         BZ    CHKSYES                                                          
         XC    ELOCKYM,ELOCKYM     CLEAR THE LOCK DATES                         
         B     CHKS60                                                           
*                                                                               
CHKS05   CLC   ESTSTAT(4),=C'LOCK'                                              
         BNE   CHKS10                                                           
         CLI   SVACT,C'A'          INVALID FOR ADD                              
         BE    CHKSNO                                                           
         TM    ECNTRL,X'04'        CAN'T LOCK HELD EST                          
         BNZ   CHKSNO                                                           
         TM    ECNTRL,X'08'                                                     
         BO    CHKSYES             NO CHANGE IN STATUS                          
         OI    ECNTRL,X'08'                                                     
         OI    KEY+13,X'08'                                                     
         XC    ELOCKYM,ELOCKYM     CLEAR LOCK DATES                             
         B     CHKS60              STATUS CHANGE - NO EDITS                     
*                                                                               
CHKS10   CLC   ESTSTAT(4),=C'HOLD'                                              
         BNE   CHKS20                                                           
         CLI   SVACT,C'A'          INVALID FOR ADD                              
         BE    CHKSNO                                                           
         TM    ECNTRL,X'0C'                                                     
         BO    CHKSYES             NO CHANGE IN STATUS                          
         OI    ECNTRL,X'0C'                                                     
         OI    KEY+13,X'0C'                                                     
         XC    ELOCKYM,ELOCKYM     CLEAR LOCK DATES                             
         B     CHKS60              STATUS CHANGE - NO EDITS                     
*                                                                               
CHKS20   CLC   ESTSTAT(6),=C'UNLOCK'                                            
         BNE   CHKS30                                                           
         CLI   SVACT,C'A'          INVALID FOR ADD                              
         BE    CHKSNO                                                           
         TM    ECNTRL,X'04'        CAN'T UNLOCK HELD EST                        
         BNZ   CHKSNO                                                           
         TM    ECNTRL,X'08'                                                     
         BZ    CHKSNO              EST WASN'T LOCKED                            
         NI    ECNTRL,X'F7'                                                     
         NI    KEY+13,X'F7'                                                     
         B     CHKS60              STATUS CHANGE - NO EDITS                     
*                                                                               
CHKS30   CLC   AGENCY,=C'JW'       FOR JWT, DO NOT ALLOW REL, ONLY DBT          
         BNE   CHKS40                                                           
         CLC   ESTSTAT(3),=C'DBT'                                               
         BE    CHKS50                                                           
         B     CHKS55                                                           
*                                                                               
CHKS40   CLC   ESTSTAT(3),=C'REL'  RELEASE FMT = RELNN                          
         BNE   CHKS55              NN=TODAY'S DAY                               
*                                                                               
CHKS50   CLI   SVACT,C'A'          INVALID FOR ADD                              
         BE    CHKSNO                                                           
         CLI   5(R2),5                                                          
         BNE   CHKSNO                                                           
         GOTO1 VDATCON,DMCB,(5,0),(0,WORK)                                      
         CLC   WORK+4(2),11(R2)    DAYS MUST MATCH                              
         BNE   CHKSNO                                                           
         NI    ECNTRL,X'F3'                                                     
         NI    KEY+13,X'F3'                                                     
         B     CHKS60              STATUS CHANGE - NO EDITS                     
*                                                                               
CHKS55   DS    0H              CHECK IF FORMAT IS M/Y, M/Y-, OR M/Y+            
         MVI   WORK,0          USE FIRST BIT X'80'="-" OR X'40'="+"             
         LA    R1,WORK+1       FIND AND REMOVE + OR - AFTER DATE                
         LR    R4,R1                                                            
         MVC   WORK+1(L'ESTSTAT),ESTSTAT                                        
         LA    R3,L'ESTSTAT                                                     
CHKS55A  CLI   0(R1),C'-'                                                       
         BNE   *+12                                                             
         MVI   WORK,X'80'      SAVE BIT TO SHOW IT IS PRIOR                     
         B     CHKS55C                                                          
         CLI   0(R1),C'+'                                                       
         BNE   *+12                                                             
         MVI   WORK,X'40'      SAVE BIT TO SHOW IT IS SUBSEQ                    
         B     CHKS55C                                                          
         CLI   0(R1),C' '                                                       
         BE    CHKS55F                                                          
         CLI   0(R1),X'0'                                                       
         BE    CHKS55F                                                          
         CLI   0(R1),C'/'                                                       
         BE    CHKS55B                                                          
         CLI   0(R1),C'A'                                                       
         BL    CHKS80          NOT A VALID DATE                                 
         CLI   0(R1),C'9'                                                       
         BH    CHKS80          NOT A VALID DATE                                 
CHKS55B  LA    R1,1(R1)                                                         
         BCT   R3,CHKS55A                                                       
         B     CHKS55F         A + OR - WAS NOT FOUND                           
*                                                                               
CHKS55C  MVI   0(R1),C' '      REPLACE + OR - WITH A BLANK FOR DATVAL           
*                                                                               
CHKS55F  DS    0H                                                               
         GOTO1 VDATVAL,DMCB,(2,WORK+1),WORK+20                                  
         OC    DMCB(4),DMCB                                                     
         BZ    CHKS80                                                           
         TM    ECNTRL,X'04'        CAN'T USE DATE IF HELD ESTIMATE              
         BNZ   CHKSNO                                                           
         TM    ECNTRL,X'08'        CAN'T USE DATE IF LOCKED ESTIMATE            
         BNZ   CHKSNO                                                           
*                                                                               
* CHECK IF MONTH IS IN ESTIMATE PERIOD                                          
         MVI   ERRCD,NOTINEST                                                   
         CLC   WORK+20(4),ESTART                                                
         BL    CHKSNO                                                           
         CLC   WORK+20(4),EEND                                                  
         BH    CHKSNO                                                           
         MVI   ERRCD,INVERR                                                     
*                                                                               
         GOTO1 VDATCON,DMCB,(0,WORK+20),(3,WORK+30)                             
         OC    WORK+31(1),WORK     SET AS PRIOR OR SUBSEQ                       
         CLC   ELOCKYM,WORK+30     CHECK IF ALREADY IN RECORD                   
         BE    CHKSYES             YES, THEN NO CHANGE IN STATUS                
         MVC   ELOCKYM,WORK+30     ADD YM LOCK DATE TO RECORD                   
         OC    ELOCKMON,WORK       SET AS PRIOR OR SUBSEQ                       
*                                                                               
CHKS60   GOTO1 PUTREC                                                           
         GOTO1 CNCHASPT                                                         
         MVC   COMMAND,=C'DMREAD'                                               
         GOTO1 DIR                                                              
         MVC   KEY+13(1),ECNTRL                                                 
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   CHKS70              NO                                           
         CLI   SVEBCMED,C'T'       TV                                           
         BNE   CHKS70              NO                                           
*                                  NEED TO WRITE BACK NWRK AND COMB             
*                                  POINTERS                                     
         MVC   KEY,SVKEY                                                        
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'03'         NWRK                                         
         MVC   COMMAND,=C'DMREAD'                                               
         GOTO1 DIR                                                              
         MVC   KEY+13(1),ECNTRL                                                 
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
         MVC   KEY,SVKEY                                                        
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'08'         COMB                                         
         MVC   COMMAND,=C'DMREAD'                                               
         GOTO1 DIR                                                              
         MVC   KEY+13(1),ECNTRL                                                 
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
*                                                                               
CHKS70   MVC   KEY,SVKEY                                                        
         MVI   WORK,C'S'           SET FOR STATUS CHG                           
         B     CHKSYES             SO I WON'T REFORMAT REC                      
*                                                                               
CHKS80   BAS   RE,CKLINID          TEST TERMINAL AUTH                           
         BE    CHKS90              ALLOW CHG OF OLD-NEW                         
         CLI   SVACT,C'A'          ELSE MUST BE ADD                             
         BNE   CHKSNO                                                           
*                                                                               
CHKS90   NI    EPRDCD,X'7F'        SET OFF X'80'                                
         CLC   ESTSTAT(3),=C'OLD'                                               
         BE    CHKSYES                                                          
         CLC   ESTSTAT(3),=C'NEW'                                               
         BNE   CHKSNO                                                           
         CLI   SVEBCMED,C'N'       ONLY FOR NETWORK                             
         BNE   CHKSNO                                                           
         OI    EPRDCD,X'80'                                                     
*                                                                               
CHKSYES  SR    RC,RC                                                            
CHKSNO   MVC   ERRNUM,=AL2(ERROR)                                               
         LTR   RC,RC                                                            
         BNE   SPERREX                                                          
         XIT1                                                                   
*                                                                               
*****************************************************************               
*        CHECK MASTER TERMINAL/LINE ID                          *               
*****************************************************************               
CKLINID  NTR1                                                                   
         LA    R1,LINIDTAB           MASTER TERMINAL LINE ID +ADDR              
CKL10    CLI   0(R1),X'FF'                                                      
         BE    CKLNO                                                            
         CLC   LINID(8),0(R1)                                                   
         BE    CKLYES                ALLOW CHG OF OLD-NEW                       
         LA    R1,8(R1)                                                         
         B     CKL10                                                            
CKLYES   SR    R1,R1                                                            
CKLNO    LTR   R1,R1                                                            
         XIT1                                                                   
*                                                                               
*********************************************************************           
* CHKEDTS- FOR NETWORK CAN'T HAVE MORE THAN 12 CALENDAR MONTHS      *           
*          FOR SPOT CAN'T HAVE MORE THAN 12 BROADCAST MONTHS        *           
*            OR 53 WEEKS (WE THINK)                                 *           
*********************************************************************           
*                                                                               
CHKEDTS  NTR1                                                                   
         MVC   WORK+6(6),SYR                                                    
         CLI   OVSYS,2               SPOT?                                      
         BNE   CHKEDT02                                                         
         GOTO1 GETBRD,DMCB,(1,SYR),WORK,VGETDAY,VADDAY                          
CHKEDT02 GOTO1 VDATCON,DMCB,WORK+6,(3,DUB)                                      
*                                                                               
         MVC   WORK+6(6),EYR                                                    
         CLI   OVSYS,2               SPOT?                                      
         BNE   CHKEDT06                                                         
         CLC   SYR(2),EYR            TEST SAME YEAR                             
         BE    CHKEDT04                                                         
         CLC   SYR+2(4),EYR+2        THEN START MMMDD MUST BE > END             
         BNH   CHKEDTN                                                          
*                                                                               
         GOTO1 DATCON,DMCB,SYR,(3,DUB+6)                                        
         ZIC   R4,DUB+6              EST START YR                               
         GOTO1 DATCON,DMCB,EYR,(3,DUB+6)                                        
         ZIC   R5,DUB+6              EST END YR                                 
         SR    R5,R4                                                            
         CH    R5,=H'2'              MAKE SURE EST NOT > 365 DAYS               
         BNL   CHKEDTN                                                          
*                                                                               
CHKEDT04 GOTO1 GETBRD,DMCB,(1,EYR),WORK                                         
CHKEDT06 GOTO1 DATCON,DMCB,WORK+6,(3,DUB+3)                                     
         ZIC   R4,DUB                                                           
         ZIC   R5,DUB+3                                                         
         SR    R5,R4                                                            
         BZ    CHKEDTY               SAME YEAR IS ALWAYS OK                     
         CH    R5,=H'1'                                                         
         BH    CHKEDTN               CAN'T EXCEED ONE YEAR (SO INVALID)         
         CLC   DUB+1(2),DUB+4        NELSON SAYS TO ALLOW THIS                  
         BNH   CHKEDTN               SO LONG AS NOT MORE THAN 365 DAYS          
         CLI   OVSYS,2               SPOT?                                      
         BNE   CHKEDTY               DONE FOR NET                               
         CLC   DUB+1(1),DUB+4        INVALID IF SAME BRD MONTH (DIF YRS)        
         BE    CHKEDTN               CAN'T HAVE 13 BRD MONTHS                   
         B     CHKEDTY                                                          
*                                                                               
CHKEDTN  CLC   =C'SJ',AGENCY         SJ HAS NO TALENT RECORDS                   
         BE    *+12                                                             
         BAS   RE,CHKTAL                                                        
         BE    CHKEDTY               NO RESTRIC ON DATES- TAL REC EXISTS        
         MVC   ERRNUM,=AL2(ERROR)    (INVALID DATE SPREAD)                      
         LTR   R3,R3                 DATES INVALID                              
         BNZ   SPERREX                                                          
*                                                                               
CHKEDTY  MVC   ERRNUM,=AL2(ERROR)    (INVALID DATE SPREAD)                      
         LA    R2,ESTSTRDH           CURSOR TO START DATE                       
         CR    R3,R3                                                            
         BNZ   SPERREX                                                          
CHKEDTX  XIT1                                                                   
************************************************************                    
* CHKTAL - CHECKS IF TALENT FACTOR RECORD EXISTS           *                    
*        - EX DF HAS CHILD SPOT ESTIMATES THAT CAN BE LONG *                    
*          SETS CC = IF IT EXISTS                          *                    
************************************************************                    
*                                                                               
CHKTAL   NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D27'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),BCLT                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(5),KEYSAVE                                                   
         XIT1                                                                   
*                                                                               
***********************************************************************         
*                          ERROR MESSAGES                             *         
***********************************************************************         
*                                                                               
ERRINV   MVI   ERROR,INVALID                                                    
         B     VSFMERR                                                          
ERRMIS   MVI   ERROR,MISSING                                                    
         B     VSFMERR                                                          
ERRBKLN  MVC   ERRNUM,=AL2(BKLNINV)                                             
         B     SPERREX                                                          
*                                                                               
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
VSFMERR  MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         DROP  RF                                                               
         SPACE 2                                                                
*                                  SHORT DESP OF ERROR MSGS                     
BKLNINV  EQU   449                 BREAK LN MUST BE 1-4                         
GMIERR   EQU   486                 GMI NOT SET UP FOR CLIENT                    
BIGERR   EQU   487                 OTHER AGENCY FEE TOO LARGE                   
TALERR   EQU   488                 NTP NOT VALID FOR PRODUCT                    
TALREQ   EQU   489                 NTP REQUIRED FOR THIS CLIENT                 
CLTFULL  EQU   485                 TOO MANY PRODUCTS                            
PRDERR   EQU   491                 PRD ERROR CHECK TRAFFIC MASTER PRD           
SCRTYERR EQU   492                 ACCESS TO CLIENT NOT AUTHORIZED              
ERRTRADE EQU   493                 TRADE PRD CODE ALREAD IN CLIST               
ERRTRAD2 EQU   494                 THIRD CHAR IN PRD CODE MUST BE 'C'           
CLNTERR  EQU   40                  CLIENT NOT FOUND                             
PRODERR  EQU   41                  PRODUCT NOT FOUND                            
NTPERR   EQU   495                 NTP MUST FIT BETWEEN 0 AND 2                 
BADDATE  EQU   20                  INVALID DATE                                 
PNREQER  EQU   496                 PRD NAME REQUIRED                            
PNDELER  EQU   497                 PRD NAME CANNOT BE DELETE                    
CPERR1   EQU   498                 CLT/PRD CODE REQUIRED                        
CPERR2   EQU   499                 CLT/PRD CODE MUST BE 4 ALPHANUMER            
CPERR3   EQU   503                 CLT/PRD CODE MUST BE 4 NUMERICS              
CPERR4   EQU   505                 CLT/PRD CODE MUST BE 5 NUMERICS              
PCLERR1  EQU   506                 CLASSES MUST BE A - I                        
PCLERR2  EQU   507                 CLASSES CANNOT BE EQUAL                      
BTNERR   EQU   508                 BILL-TO-NAME REQUIRED                        
OAFERR   EQU   509                 OAF CANNOT EXCEED 9.99                       
BBERR    EQU   510                 BILL-BASIS IS CGROSS OR CNET                 
COMPERR1 EQU   511                 COM % REQUIRED                               
COMPERR2 EQU   515                 COM % VALID NUMERIC                          
COMPERR3 EQU   512                 100% IS MAX COM %                            
COMPERR4 EQU   513                 0% IS MIN COM %                              
COMPERR5 EQU   514                 1ST CHAR IS + OR -                           
CBASERR1 EQU   522                 COMM BASIS REQUIRED                          
CBASERR2 EQU   521                 COMM BASIS GROSS OR NET                      
EDERR1   EQU   542                 DATE REQUIRES BILL BASIS                     
EDERR2   EQU   523                 DATE REQUIRED FOR BILL BASIS                 
GSTERR   EQU   524                 INVALID GST CODE                             
PSTERR1  EQU   528                 INVALID PST CODE                             
EDTERR1  EQU   530                 INPUT CANNOT BE NUMERIC                      
EDTERR2  EQU   529                 INPUT MUST BE NUMERIC                        
OPTERR1  EQU   531                 OPTION MUST BE NTP                           
OPTERR2  EQU   532                 NTP MUST BE 0-2                              
VKERR1   EQU   533                 MUST BE 3 LONG                               
VKERR2   EQU   534                 MUST BE VALID HEX                            
VKERR3   EQU   535                 ZZZ INVALID PRD CODE                         
VKERR4   EQU   536                 CLIENT MUST BE 'CC'                          
VKERR5   EQU   537                 1ST CHAR MUST BE ALPHA                       
VKERR6   EQU   538                 MUST BE 2 OR 3 CHARS LONG                    
VKERR7   EQU   539                 2ND AND 3RD CHARS ALPHANUMERIC               
VKERR8   EQU   540                 ALL IS INVALID PRD CODE                      
VKERR9   EQU   541                 NO IS INVALID PRD CODE                       
TALERR2  EQU   546                 CANNOT CHANGE FIELD                          
NTPERR3  EQU   547                 NTP CAN ONLY BE SET ONCE                     
DELERR1  EQU   548                 PRODUCT EXISTS FOR GROUP IN ID               
DELERR2  EQU   549                 MASTER TRAFFIC CLT ALREADY EXISTS            
DELERR3  EQU   550                 BILL RECS SHOULD SUM TO ZERO                 
DELERR4  EQU   551                 ORDERED OR PAID $ ON EST                     
DELERR5  EQU   552                 BILL ON FILE- CANNOT DEL PRD                 
DELERR6  EQU   553                 NO GOALS-- GO CHECK FOR BUYS                 
DELERR7  EQU   554                 HAS BUYS, CANNOT DELETE                      
DELERR8  EQU   555                 CANNOT DELETE POL PRODUCT                    
DELERR9  EQU   560                 'DELETE' MUST BE IN PROD NAME                
PFERR    EQU   559                 INVALID PFKEY                                
         EJECT                                                                  
***********************************************************************         
*        CONSTANTS                                                    *         
***********************************************************************         
EOTBLQ   DC    C'A'                                                             
DMAX     EQU   14                                                               
*                                                                               
***********************************************************************         
*        VALID NUMERICS TABLE                                         *         
***********************************************************************         
VALDNTBL DC    C' 0123456789-/'                                                 
*                                                                               
***********************************************************************         
*        FTYPTAB                                                      *         
***********************************************************************         
FTYPTAB  DC    C'M$',X'03'                                                      
         DC    C'M%',X'04'                                                      
         DC    C'Q$',X'05'                                                      
         DC    X'FF'                                                            
***********************************************************************         
*        TARGET NUMBER LIST                                           *         
***********************************************************************         
ONETWO   DC    X'0102FF'                                                        
*                                                                               
***********************************************************************         
*        AGENCY TABLE                                                 *         
***********************************************************************         
AGYTAB   DC   C'GY'                                                             
         DC   C'DR'                                                             
         DC   C'GN'                                                             
         DC   C'CE'                                                             
         DC   C'FM'                                                             
         DC   C'RE'                                                             
         DC   X'FF'                                                             
***********************************************************************         
*        CTYPTAB                                                      *         
***********************************************************************         
CTYPTAB  DC    C'M$',X'03'                                                      
         DC    C'M%',X'04'                                                      
         DC    C'Q$',X'05'                                                      
         DC    X'FF'                                                            
***********************************************************************         
*        LINE ID TABLE                                                *         
***********************************************************************         
LINIDTAB DC    C'DD13C2D1'                                                      
         DC    C'DDL1136T'          DDS-LA                                      
         DC    C'DDL1137T'          DDS-LA                                      
         DC    C'DDL1138T'          DDS-LA                                      
         DC    C'DX06200T'         (WAS DDNY720T)                               
         DC    C'DDNY700T'                                                      
         DC    C'DDNYD03T'                                                      
*NOP*    DC    C'DX03901T'                                                      
         DC    C'DDNY916T'                                                      
*NOP*    DC    C'DDNY720T'                                                      
*NOP*    DC    C'DDNYF11T'          DDS                                         
         DC    C'HDTO847T'          HDTO (WAS HDTO823T)                         
         DC    C'HDTO829T'          HDTO (WAS HDTO830T)                         
         DC    C'XDDSC84A'                                                      
*NOP*    DC    C'DDNYD26T'                                                      
         DC    X'FF'                                                            
***********************************************************************         
*        GST TABLE                                                    *         
***********************************************************************         
GSTTAB   DC   C'S'                                                              
         DC   C'U'                                                              
         DC   C'X'                                                              
         DC   C'Z'                                                              
         DC   X'FF'                                                             
*                                                                               
***********************************************************************         
*        TAL TABLE                                                    *         
***********************************************************************         
TALTAB   DC   X'0',X'01',X'3F'                                                  
         DC   X'1',X'40',X'5F'                                                  
         DC   X'2',X'60',X'7F'                                                  
         DC   X'3',X'80',X'9F'                                                  
         DC   X'FF'                                                             
         EJECT                                                                  
***********************************************************************         
*        MEDIA TABLE                                                  *         
***********************************************************************         
MEDTAB   DC   CL1'T',XL1'01'                                                    
MEDTABLQ EQU  *-MEDTAB                                                          
         DC   CL1'R',XL1'02'                                                    
         DC   CL1'N',XL1'03'                                                    
         DC   CL1'X',XL1'04'                                                    
         DC   CL1'C',XL1'08'                                                    
         DC   X'FF'                                                             
         EJECT                                                                  
***********************************************************************         
*        PFKEYS TABLES                                                *         
***********************************************************************         
*                                                                               
PFTABLE DS   0H                                                                 
*        CLIENT MAINT DISPLAY                                                   
         DC   AL1(MPF04X-*,04,PFTCPROG,(MPF04X-MPF04)/KEYLNQ,0)                 
         DC   CL3'CM '                 MAINT                                    
         DC   CL8'FCLT'                RECORD                                   
         DC   CL8'DISP'                ACTION                                   
MPF04    DC   AL1(KEYTYTWA,L'PRDMEDK-1),AL2(PRDMEDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'PRDCLIK-1),AL2(PRDCLIK-T217FFD)                    
MPF04X   EQU  *                                                                 
*                                                                               
*        CLIENT2 MAINT DISPLAY                                                  
         DC   AL1(MPF05X-*,05,PFTCPROG,(MPF05X-MPF05)/KEYLNQ,0)                 
         DC   CL3'CM2'                 MAINT                                    
         DC   CL8'FCL2'                RECORD                                   
         DC   CL8'DISP'                ACTION                                   
MPF05    DC   AL1(KEYTYTWA,L'PRDMEDK-1),AL2(PRDMEDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'PRDCLIK-1),AL2(PRDCLIK-T217FFD)                    
MPF05X   EQU  *                                                                 
*                                                                               
*        PRODUCT LIST DISPLAY                                                   
         DC   AL1(LPF03X-*,03,PFTCPROG,(LPF03X-LPF03)/KEYLNQ,0)                 
         DC   CL3'PL '                 LIST                                     
         DC   CL8'FPRD'                RECORD                                   
         DC   CL8'LIST'                ACTION                                   
LPF03    DC   AL1(KEYTYTWA,L'PRDMEDK-1),AL2(PRDMEDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'PRDCLIK-1),AL2(PRDCLIK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'PRDPROK-1),AL2(PRDPROK-T217FFD)                    
LPF03X   EQU  *                                                                 
*                                                                               
*        CLIENT LIST DISPLAY                                                    
         DC   AL1(LPF06X-*,06,PFTCPROG,(LPF06X-LPF06)/KEYLNQ,0)                 
         DC   CL3'CL '                 LIST                                     
         DC   CL8'FCLT'                RECORD                                   
         DC   CL8'LIST'                ACTION                                   
LPF06    DC   AL1(KEYTYTWA,L'PRDMEDK-1),AL2(PRDMEDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'PRDCLIK-1),AL2(PRDCLIK-T217FFD)                    
LPF06X   EQU  *                                                                 
*                                                                               
*        RETURN CALLER                                                          
         DC    AL1(LPF12X-*,12,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
LPF12X   EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                        *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
***********************************************************************         
*        DSECTS                                                       *         
***********************************************************************         
PRODUCT DSECT                                                                   
       ++INCLUDE SPGENPRD          PRODUCT RECORD                               
         EJECT                                                                  
CLIENT  DSECT                                                                   
       ++INCLUDE SPGENCLT          CLIENT  RECORD                               
         EJECT                                                                  
       ++INCLUDE SPGENEST          ESTIMATE RECORDS                             
         EJECT                                                                  
       ++INCLUDE SPGENBILL         BILLING RECORDS                              
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFM70D          OLD MAINTENACE SCREEN                        
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPLFMF3D          EST MAINTENACE SCREEN                        
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPSFM71D          LIST SCREEN                                  
         EJECT                                                                  
       ++INCLUDE SPGENADV          ADVERTISER HEADER RECORD                     
         EJECT                                                                  
       ++INCLUDE DDPSTBLK          PROVINCIAL TAX VALIDATION                    
         EJECT                                                                  
       ++INCLUDE DDCOREQUS                                                      
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
*      ++INCLUDE DEDBLOCK                                                       
*        EJECT                                                                  
       ++INCLUDE DDFLDIND                                                       
         EJECT                                                                  
       ++INCLUDE SPGENAPY          AUTOPAY RECORD DSECT                         
         EJECT                                                                  
       ++INCLUDE DDSCANBLKD        FOR SCANNER                                  
         EJECT                                                                  
       ++INCLUDE DDOFFICED                                                      
         EJECT                                                                  
       ++INCLUDE FAGETTXTD         ERROR MSGS                                   
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
***********************************************************************         
*        SAVED STORAGE DSECT                                          *         
***********************************************************************         
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    A                   RELOCATION FACTOR                            
PROKEY   DS    CL13                                                             
ESTKEY   DS    CL13                                                             
SAVEKEY  DS    XL13                                                             
*                                                                               
SVPOLPW  DS    XL3                                                              
*                                                                               
SVCLTPOL DS    CL1                                                              
SVCLPROF DS    CL15                                                             
SVCLEX   DS    CL15                                                             
SVCLDLY  DS    CL1                                                              
SVCLTPW  DS    XL3                                                              
SVCCOST2 DS    XL4                                                              
*                                                                               
SVCLTDA  DS    XL4                                                              
SVPRDDA  DS    XL4                                                              
SVAGYFL1 DS    XL1                                                              
FADDR    DS    A                                                                
ERRAREA  DS    X                                                                
SVP1USER DS    CL20                                                             
SVP1TYPE DS    CL1                                                              
SVP1LEN  DS    XL1                                                              
SVP1FLG1 DS    XL1                                                              
SVP1FLG2 DS    XL1                                                              
*                                                                               
SVP2USER DS    CL20                                                             
SVP2TYPE DS    CL1                                                              
SVP2LEN  DS    XL1                                                              
SVP2FLG1 DS    XL1                                                              
SVP2FLG2 DS    XL1                                                              
*                                                                               
SVCLOP1  DS    XL1                                                              
SVCLOP2  DS    XL1                                                              
*                                                                               
SVPPST   DS    CL10                                                             
*                                                                               
SCRNFLAG DS    X                                                                
AUSR     DS    A                                                                
PSTOUT   DS    CL64                                                             
OPTNFLAG DS    XL1                                                              
TALOPTN  EQU   X'80'                                                            
FLAG1    DS    XL1                                                              
FLAG2    DS    XL1                                                              
LEN      DS    XL1                                                              
UTYPE    DS    CL1                                                              
USERDATA DS    CL32                                                             
*                                                                               
SVADVLST DS    CL30                                                             
FAKEFLD  DS    XL11                                                             
*                                                                               
ERRNUM   DS    XL2                                                              
SAVESEL  DS    CL1                                                              
WORK2    DS    CL48                                                             
SVADVDA  DS    CL4                                                              
SVADVAGY DS    XL1                                                              
SVADVKEY DS    XL13                                                             
BYTE2    DS    XL1                                                              
CCOUNT   DS    XL1                                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        LIST LINE DSECT                                              *         
***********************************************************************         
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR              LABELS FOR LISTMON                           
         DS    CL2                                                              
         DS    CL3                                                              
         DS    CL3                                                              
LSPRDC   DS    CL3                                                              
         DS    CL12                                                             
LSPRDN   DS    CL20                                                             
*                                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'186MZEIEST   05/01/02'                                      
         END                                                                    
