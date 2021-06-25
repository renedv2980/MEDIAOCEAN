*          DATA SET SPSFM02    AT LEVEL 009 AS OF 04/26/16                      
*PROCESS USING(WARN(15))                                                        
*PHASE T21702A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T21702  -- SUPERDESK AUTHORIZATION MAINTENANCE       *         
*                                                                     *         
*  COMMENTS:     MAINTAINS AUTHORIZATION RECORDS                      *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T21700), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREEN SCSFMC2                                       *         
*                                                                     *         
*  OUTPUTS:                                                           *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
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
         TITLE 'T21702 - SUPERDESK AUTHORIZATION MAINTENANCE'                   
T21702   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1702**,R7,RR=R3                                              
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
         CLI   MODE,DISPKEY        DISPLAY KEY (LIST)                           
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,SETFILE        SET FILES                                    
         BE    SF                                                               
         CLI   MODE,RECDEL         DELETE RECORD                                
         BE    DEL                                                              
         CLI   MODE,RECREST        RESTORE RECORD                               
         BE    RES                                                              
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       SET FILE                                      *         
***********************************************************************         
SF       BAS   RE,SSV                                                           
         B     XIT                                                              
***********************************************************************         
*                       VALIDATE KEY                                  *         
***********************************************************************         
VK       DS    0H                                                               
         BAS   RE,RSV              RESET SYSTEM VALUES                          
*                                                                               
         LA    R2,AUTMEDH          VALIDATE MEDIA                               
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY                         
         BO    VK05                                                             
         GOTO1 VALIMED                                                          
         MVC   AUTMDN,MEDNM        DISPLAY MEDIA NAME                           
         OI    AUTMDNH+6,X'80'                                                  
*                                                                               
VK05     LA    R2,AUTCLTH          VALIDATE CLIENT                              
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY                         
         BO    VK10                                                             
         XC    BCLT,BCLT                                                        
         CLI   ACTNUM,ACTLIST      NOT REQUIRED IF ACTION=LIST                  
         BNE   *+12                                                             
         CLI   5(R2),0                                                          
         BE    VK10                                                             
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         MVI   USEIONUM,2                                                       
         GOTO1 VALICLT                                                          
         MVC   AUTCLN,CLTNM        DISPLAY CLIENT NAME                          
         OI    AUTCLNH+6,X'80'                                                  
*                                                                               
VK10     LA    R2,AUTPRDH          VALIDATE PRODUCT                             
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY                         
         BO    VK25                                                             
         MVI   BPRD,0                                                           
         MVI   BPRD2,0                                                          
         CLI   ACTNUM,ACTLIST      NOT REQUIRED IF ACTION=LIST                  
         BNE   *+12                                                             
         CLI   5(R2),0                                                          
         BE    VK25                                                             
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         MVI   USEIONUM,2                                                       
         GOTO1 VALIPRD                                                          
         MVC   AUTPDN,PRDNM        DISPLAY PRODUCT NAME                         
         OI    AUTPDNH+6,X'80'                                                  
*                                                                               
         CLI   5(R2),3             IS THERE A PIGGYBACK PRODUCT?                
         BH    VK15                                                             
         MVI   BPRD2,0             CLEAR PIGGYBACK                              
         B     VK25                                                             
*                                                                               
VK15     MVC   BYTE,BPRD                                                        
         LA    R3,AUTPRD+2                                                      
         CLI   0(R3),C'-'                                                       
         BE    VK20                                                             
         LA    R3,1(R3)                                                         
         CLI   0(R3),C'-'                                                       
         BNE   ERRINV                                                           
VK20     LA    R2,TEMPFLDH                                                      
         MVC   8(3,R2),1(R3)                                                    
         OC    8(3,R2),SPACES                                                   
         MVI   5(R2),3             INPUT LENGTH FOR FAKE FIELD                  
         GOTO1 VALIPRD                                                          
         MVC   BPRD2,BPRD                                                       
         MVC   BPRD,BYTE                                                        
*                                                                               
VK25     LA    R2,AUTESTH          VALIDATE ESTIMATE                            
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY                         
         BO    VK30                                                             
         MVI   BEST,0                                                           
         CLI   ACTNUM,ACTLIST      NOT REQUIRED IF ACTION=LIST                  
         BNE   *+12                                                             
         CLI   5(R2),0                                                          
         BE    VK30                                                             
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         MVI   USEIONUM,2                                                       
         GOTO1 VALIEST                                                          
         L     R6,AIO              DISPLAY ESTIMATE DATES                       
         USING ESTHDR,R6                                                        
         GOTO1 DATCON,DMCB,(0,ESTART),(10,AUTESD)                               
         GOTO1 DATCON,DMCB,(0,EEND),(10,AUTESD+11)                              
         MVI   AUTESD+9,C'-'                                                    
         OI    AUTESDH+6,X'80'                                                  
         DROP  R6                                                               
*                                                                               
VK30     LA    R2,AUTSTAH          IS THIS A STATION RECORD?                    
         CLI   5(R2),0             DON'T NEED TO VALIDATE MARKET                
         BNE   VK40                                                             
*                                                                               
         LA    R2,AUTMKTH          IS THIS A MARKET RECORD?                     
         CLI   5(R2),0                                                          
         BNE   VK35                                                             
         XC    AUTMKN,AUTMKN       NO, CLEAR MARKET NAME                        
         OI    AUTMKNH+6,X'80'                                                  
         B     VK45                                                             
*                                                                               
VK35     TM    4(R2),X'20'         MKT RECORD VALIDATED PREVIOUSLY?             
         BO    VK45                                                             
         XC    BMKT,BMKT                                                        
         MVI   USEIONUM,2                                                       
         GOTO1 VALIMKT                                                          
         MVC   AUTMKN,MKTNM                                                     
         OI    AUTMKNH+6,X'80'                                                  
         B     VK45                                                             
*                                                                               
VK40     LA    R2,AUTSTAH          VALIDATE STATION                             
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY                         
         BO    VK45                                                             
         GOTO1 VALISTA                                                          
         MVC   AUTMKT,QMKT                                                      
         OI    AUTMKTH+6,X'80'                                                  
         MVC   AUTMKN,MKTNM                                                     
         OI    AUTMKNH+6,X'80'                                                  
*                                                                               
VK45     LA    R2,AUTVERH          VALIDATE VERSION NUMBER                      
         CLI   ACTNUM,ACTLIST      NOT REQUIRED IF ACTION=LIST                  
         BE    VK50                                                             
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
*                                                                               
VK50     TM    4(R2),X'20'         VALIDATED PREVIOUSLY                         
         BO    VK70                                                             
         MVI   SVVERSN,0                                                        
         CLI   AUTVER,0                                                         
         BE    VK70                                                             
*                                                                               
         TM    4(R2),X'04'         INPUT IS ALPHABETIC?                         
         BNO   ERRINV                                                           
         CLI   5(R2),1                                                          
         BNE   ERRINV                                                           
         ZAP   DUB,=P'0'                                                        
         LA    R3,VERSTAB                                                       
VK60     CLI   0(R3),C' '                                                       
         BE    ERRINV                                                           
         CLC   AUTVER,0(R3)                                                     
         BE    VK65                                                             
         LA    R3,1(R3)                                                         
         AP    DUB,=P'1'           INCREMENT COUNTER                            
         B     VK60                                                             
*                                                                               
VK65     MVC   HALF,DUB+6                                                       
         ZAP   WORK(2),=P'99'            FINDING 9'S COMPLIMENT                 
         SP    WORK(2),HALF              99 - (REV #) = 9'S COMP REV #          
         SRP   WORK(2),1,0               SHIFT LEFT 1 DIGIT                     
         MVC   SVVERSN,WORK                                                     
*                                                                               
* ONLY VALIDATE HERE IF A MARKET OR STATION AUTH RECORD AND NOT LIST            
VK70     CLI   ACTNUM,ACTLIST                                                   
         BNE   VK75                                                             
         MVI   SVREV,0                                                          
         LA    R2,LISOPTH                                                       
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY                         
         BO    VK90                                                             
         NI    FLAG1,X'FF'-FL1NOSTA-FL1NOMKT                                    
*                                                                               
         CLI   5(R2),0             ANYTHING ENTERED?                            
         BE    VK90                NO, OK                                       
         CLC   =C'NOSTA',LISOPT          SUPPRESS STATIONS?                     
         BNE   *+12                                                             
         OI    FLAG1,FL1NOSTA                                                   
         B     VK90                                                             
         CLC   =C'NOMKT',LISOPT          SUPPRESS MARKETS?                      
         BNE   ERRINV                                                           
         OI    FLAG1,FL1NOMKT                                                   
         B     VK90                                                             
*                                                                               
VK75     LA    R2,AUTRENH                                                       
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY                         
         BO    VK90                                                             
         MVI   SVREV,0                                                          
         CLI   AUTMKTH+5,0         LOOKING AT MARKET/STATION RECORD?            
         BE    VK85                                                             
         CLI   5(R2),0             CANNOT HAVE REVISION NUMBER IN               
         BNE   ERRINV              MARKET OR STATION RECORDS                    
         B     VK90                                                             
*                                                                               
VK85     CLI   5(R2),0             MUST HAVE A REV#                             
         BE    ERRMIS                                                           
         MVI   SVREV,X'99'         DEFAULT REVISION NUMBER                      
         CLI   AUTREN,C'0'                                                      
         BE    VK90                                                             
*                                                                               
         TM    4(R2),X'08'         INPUT IS VALID NUMERIC?                      
         BNO   ERRINV                                                           
         ZIC   R5,5(R2)            INPUT LENGTH                                 
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,AUTREN(0)                                                    
*                                                                               
         MVC   HALF,DUB+6                                                       
         ZAP   WORK(2),=P'99'            FINDING 9'S COMPLIMENT                 
         SP    WORK(2),HALF              99 - (REV #) = 9'S COMP REV #          
         SRP   WORK(2),1,0               SHIFT LEFT 1 DIGIT                     
         MVC   SVREV,WORK                                                       
*                                                                               
* BUILD KEY FOR GENCON                                                          
VK90     BAS   RE,SSV              SET SYSTEM VALUES (XSPDIR AND XSPF)          
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING AUTRECD,R4                                                       
         MVC   AUTKTYP(2),=X'0D39' RECORD TYPE                                  
         MVC   AUTKAM,BAGYMD       BINARY AGENCY MEDIA                          
         MVC   AUTKCLT,BCLT                                                     
         MVC   AUTKPRD,BPRD                                                     
         MVC   AUTKPRD2,BPRD2                                                   
         MVC   AUTKEST,BEST                                                     
         MVC   AUTKAUN,SVVERSN                                                  
         CLI   AUTMKTH+5,0                                                      
         BE    *+10                                                             
         MVC   AUTKMKT,BMKT                                                     
         CLI   AUTSTAH+5,0                                                      
         BE    *+10                                                             
         MVC   AUTKSTA,BSTA                                                     
         MVC   AUTKREV,SVREV                                                    
*                                                                               
         DROP  R4                                                               
*                                                                               
         MVC   AUTHKEY,KEY         SAVE COPY OF KEY                             
         MVC   AIO,AIO1                                                         
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE RECORD                               *         
***********************************************************************         
VR       L     R6,AIO                                                           
         USING AUTRECD,R6                                                       
*                                                                               
VR10     OC    KEY+AUTKSTA-AUTRECD(3),KEY+AUTKSTA-AUTRECD                       
         BZ    VR20                                                             
         BAS   RE,VALSTA           VALIDATE STATION RECORD                      
         B     VRX                                                              
VR20     OC    KEY+AUTKMKT-AUTRECD(2),KEY+AUTKMKT-AUTRECD                       
         BZ    VR30                                                             
         BAS   RE,VALMKT           VALIDATE MARKET RECORD                       
         B     VRX                                                              
VR30     BAS   RE,VALAUTH          VALIDATE AUTHORIZATION RECORD                
*                                                                               
VRX      B     DR                  REDISPLAY RECORD                             
         EJECT                                                                  
***********************************************************************         
*        VALIDATE AUTHORIZATION RECORD                                *         
***********************************************************************         
VALAUTH  NTR1                                                                   
         MVC   AUTRAGYA,TWAAGY     ALPHA AGY                                    
*                                                                               
         CLI   ACTEQU,ACTADD                                                    
         BE    VAUTH30                                                          
*                                                                               
* FIRST TEST IF JUST A REQUEST TO RUN THE OVERNIGHT                             
         LA    R2,AUTROVH          RUN OVERNIGHT?                               
         CLI   5(R2),0                                                          
         BE    VAUTH30                                                          
         CLI   AUTROV,C'Y'                                                      
         BNE   VAUTH30                                                          
         MVI   ELCODE,AINFELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   VAUTH10                                                          
         USING AINFELD,R6                                                       
         OI    AINFFLAG,AINFDTEX                                                
         B     VAUTH20                                                          
         DROP  R6                                                               
*                                                                               
VAUTH10  XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING AINFELD,R3                                                       
         MVI   AINFEL,AINFELQ      ELEMENT CODE                                 
         MVI   AINFLEN,AINFLENQ    LENGTH                                       
         OI    AINFFLAG,AINFDTEX                                                
         GOTO1 ADDELEM             ADDING "02" ELEM                             
         DROP  R3                                                               
*                                                                               
VAUTH20  GOTO1 PUTREC                                                           
         L     R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,AINFELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING AINFELD,R6                                                       
         NI    AINFFLAG,X'FF'-AINFDTEX                                          
         B     VAUTHX                                                           
         DROP  R6                                                               
*                                                                               
VAUTH30  LA    R2,AUTPWDH          VALIDATE THE PASSWORD FOR CHANGING           
*                                                                               
         CLI   T217FFD+1,C'*'      DDS TERMINAL?                                
         BNE   VAUTH31             NO                                           
         CLC   =C'CCG',AUTPWD      CCG PASSWORD?                                
         BNE   VAUTH31             NO                                           
         CLI   ACTEQU,ACTSEL       CHANGE ONLY?  NOT SURE Y ACTSEL              
         BNE   ERRAUTH             NO                                           
         BRAS  RE,GETCKSM        ONLY ALLOW THEM TO CHANGE ENDDDATE!!           
         MVC   SVCKSM,FULL                                                      
         B     VAUTH35                                                          
*                                                                               
*                                                                               
VAUTH31  BAS   RE,VALPW                                                         
         BNE   ERRAUTH                                                          
*                                                                               
VAUTH35  L     R6,AIO                                                           
         USING AUTRECD,R6                                                       
         MVC   SVDUEDT,AUDDUEDT    SAVE OFF DUEDATE AND                         
         MVC   SVFLSDT,AUDFLST     FLIGHT START DATE                            
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'01'        PRIMARY DATE ELEMENT                         
         GOTO1 REMELEM                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING AUDEL,R3                                                         
         MVI   AUDEL,1             ELEMENT CODE                                 
         MVI   AUDLEN,AUDLENQ      LENGTH                                       
*                                                                               
         MVI   ELEMFLAG,0          INITIALIZE DATE ELEM FLAG                    
*                                                                               
         LA    R2,AUTRDAH          REVISION DATE                                
         CLI   5(R2),0                                                          
         BNE   *+14                                                             
         MVC   AUDRVDT,BTODAY      USE TODAY'S DATE                             
         B     VAUTH40                                                          
         GOTO1 DATVAL,DMCB,(0,8(R2)),DATETEMP                                   
         OC    DMCB(4),DMCB                                                     
         BZ    ERRINV                                                           
         GOTO1 DATCON,DMCB,(0,DATETEMP),(3,AUDRVDT)                             
*                                                                               
VAUTH40  LA    R2,AUTBISH          BUY ISSUE DATE                               
         CLI   5(R2),0                                                          
         BE    VAUTH50                                                          
         GOTO1 DATVAL,DMCB,(0,8(R2)),DATETEMP                                   
         OC    DMCB(4),DMCB                                                     
         BZ    ERRINV                                                           
         GOTO1 DATCON,DMCB,(0,DATETEMP),(3,AUDISDT)                             
*                                                                               
VAUTH50  LA    R2,AUTBFSH          BUY FLIGHT START DATE                        
         MVC   AUDFLST,SVFLSDT                                                  
         TM    1(R2),X'20'         IS FIELD PROTECTED?                          
         BO    VAUTH60             YES, THEN SKIP THIS                          
         CLI   5(R2),0                                                          
         BE    ERRMIS              REQUIRED INPUT                               
         GOTO1 DATVAL,DMCB,(0,8(R2)),DATETEMP                                   
         OC    DMCB(4),DMCB                                                     
         BZ    ERRINV                                                           
         GOTO1 DATCON,DMCB,(0,DATETEMP),(3,AUDFLST)                             
         MVC   SVFLSDT,AUDFLST                                                  
*                                                                               
VAUTH60  LA    R2,AUTBFEH          BUY FLIGHT END DATE                          
         CLI   5(R2),0                                                          
         BE    ERRMIS              REQUIRED INPUT                               
         GOTO1 DATVAL,DMCB,(0,8(R2)),DATETEMP                                   
         OC    DMCB(4),DMCB                                                     
         BZ    ERRINV                                                           
         GOTO1 DATCON,DMCB,(0,DATETEMP),(3,AUDFLEN)                             
*                                                                               
VAUTH70  LA    R2,AUTBDUH          BUY DUE DATE                                 
         MVC   AUDDUEDT,SVDUEDT    SAVE OFF DUEDATE AND                         
         TM    1(R2),X'20'         IS FIELD PROTECTED?                          
         BO    VAUTH75             YES, THEN SKIP THIS                          
         CLI   5(R2),0                                                          
         BE    ERRMIS              REQUIRED INPUT                               
         GOTO1 DATVAL,DMCB,(0,8(R2)),DATETEMP                                   
         OC    DMCB(4),DMCB                                                     
         BZ    ERRINV                                                           
         GOTO1 DATCON,DMCB,(0,DATETEMP),(3,AUDDUEDT)                            
         MVC   SVDUEDT,AUDDUEDT                                                 
*                                                                               
VAUTH75  GOTO1 ADDELEM             ADDING "01" ELEM                             
         DROP  R3                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* SECONDARY DATE ELEMENT                                              *         
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
         MVI   ELCODE,X'02'        SECONDARY DATE ELEMENT                       
         GOTO1 REMELEM                                                          
         MVI   ELEMFLAG,C' '                                                    
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING A2DELD,R3                                                        
         MVI   A2DEL,2             ELEMENT CODE                                 
         MVI   A2DLEN,A2DLENQ      LENGTH                                       
*                                                                               
         LA    R2,AUTPPRH          PRE-POST DATE                                
         CLI   5(R2),0                                                          
         BE    VAUTH80                                                          
         MVI   ELEMFLAG,C'*'                                                    
         GOTO1 DATVAL,DMCB,(0,8(R2)),DATETEMP                                   
         OC    DMCB(4),DMCB                                                     
         BZ    ERRINV                                                           
         GOTO1 DATCON,DMCB,(0,DATETEMP),(3,A2DPRPOS)                            
*                                                                               
VAUTH80  LA    R2,AUTTPPH          TIME PERIOD POST DUE DATE                    
         CLI   5(R2),0                                                          
         BE    VAUTH90                                                          
         MVI   ELEMFLAG,C'*'                                                    
         GOTO1 DATVAL,DMCB,(0,8(R2)),DATETEMP                                   
         OC    DMCB(4),DMCB                                                     
         BZ    ERRINV                                                           
         GOTO1 DATCON,DMCB,(0,DATETEMP),(3,A2DTIME)                             
*                                                                               
VAUTH90  LA    R2,AUTAFFH          POSTING AFFIDAVIT DATE                       
         CLI   5(R2),0                                                          
         BE    VAUTH100                                                         
         MVI   ELEMFLAG,C'*'                                                    
         GOTO1 DATVAL,DMCB,(0,8(R2)),DATETEMP                                   
         OC    DMCB(4),DMCB                                                     
         BZ    ERRINV                                                           
         GOTO1 DATCON,DMCB,(0,DATETEMP),(3,A2DAFFID)                            
*                                                                               
VAUTH100 CLI   ELEMFLAG,C'*'       CK FOR "02" ELEM EMPTY OR NOT                
         BNE   VAUTH110            "02" ELEM IS EMPTY                           
         GOTO1 ADDELEM             ADDING "02" ELEM                             
         DROP  R3                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* INFO ELEMENT                                                        *         
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
VAUTH110 MVI   ELCODE,X'04'                                                     
         BRAS  RE,GETEL                                                         
         BNE   VAUTH120                                                         
         USING AINFELD,R6                                                       
         MVC   BYTE,AINFFLAG                                                    
         MVC   HALF,AINFBYGR                                                    
         MVC   FULL,AINFUSCD                                                    
         DROP  R6                                                               
         GOTO1 REMELEM                                                          
*                                                                               
VAUTH120 XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING AINFELD,R3                                                       
         MVI   AINFEL,AINFELQ      ELEMENT CODE                                 
         MVI   AINFLEN,AINFLENQ    LENGTH                                       
         CLI   ACTEQU,ACTADD                                                    
         BE    VAUTH130                                                         
         MVC   AINFFLAG,BYTE                                                    
         MVC   AINFBYGR,HALF                                                    
         MVC   AINFUSCD,FULL                                                    
*                                                                               
VAUTH130 MVI   AINFSPRQ,C'Y'                                                    
         LA    R2,AUTAPPH          SUPERVISOR APP REQUIRD? Y/N                  
         CLI   5(R2),0                                                          
         BE    VAUTH140            REQUIRED INPUT, DEFAULT = "Y"                
         CLI   AUTAPP,C'Y'                                                      
         BE    VAUTH140                                                         
         CLI   AUTAPP,C'N'                                                      
         BNE   ERRINV                                                           
         MVI   AINFSPRQ,C'N'                                                    
*                                                                               
VAUTH140 MVI   AINFBYBS,C'0'                                                    
         LA    R2,AUTBBAH          BUY BASIS                                    
         CLI   5(R2),0                                                          
         BE    VAUTH150                                                         
         CLI   AUTBBA,C'G'                                                      
         BE    VAUTH150                                                         
         MVI   AINFBYBS,C'1'                                                    
         CLI   AUTBBA,C'D'                                                      
         BNE   ERRINV                                                           
*                                                                               
VAUTH150 DS    0H                                                               
         MVC   AINFBYGR,AUTSPV                                                  
         MVC   AINFUSCD,AUTSPV+3                                                
*                                                                               
         MVI   BYTE,0              MAKEGOODS POLICY                             
         LA    R5,MGTAB                                                         
VAUTH160 CLI   0(R5),X'FF'                                                      
         BE    VAUTH180                                                         
         ZICM  R2,0(R5),2                                                       
         AR    R2,RA                                                            
         CLI   5(R2),0                                                          
         BE    VAUTH170                                                         
         CLI   8(R2),C'N'                                                       
         BE    VAUTH170                                                         
         CLI   8(R2),C'Y'                                                       
         BNE   ERRINV                                                           
         CLI   BYTE,1              IS FLAG ALREADY SET?                         
         BE    ERRINV                                                           
         MVC   AINFMG,2(R5)                                                     
         MVI   BYTE,1                                                           
*                                                                               
VAUTH170 LA    R5,3(R5)                                                         
         B     VAUTH160                                                         
*                                                                               
VAUTH180 CLI   BYTE,0              END OF TABLE, ERROR IF NO INPUT              
         BE    ERRMIS                                                           
VAUTH190 LA    R2,AUTWMGH          WEEKS MAKEGOODS CAN RUN                      
         CLI   5(R2),0                                                          
         BE    VAUTH200                                                         
         TM    4(R2),X'08'         INPUT IS VALID NUMERIC?                      
         BNO   ERRINV                                                           
         ZIC   R5,5(R2)            INPUT LENGTH                                 
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,AUTWMG(0)                                                    
         SR    R5,R5                                                            
         CVB   R5,DUB                                                           
         CH    R5,=H'255'                                                       
         BH    ERRINV              CANNOT BE GREATER THAN 1 BYTE                
         STCM  R5,1,AINFMGWK                                                    
*                                                                               
VAUTH200 GOTO1 ADDELEM             ADDING "02" ELEM                             
         DROP  R3                                                               
         CLI   ACTEQU,ACTADD                                                    
         BNE   VAUTHX                                                           
*                                                                               
         LA    R6,KEY                                                           
         USING AUTRECD,R6                                                       
         XC    KEY,KEY                   ADD '0DB9' PASSIVE KEY                 
         MVC   AUPKTYP(2),=X'0DB9'                                              
         MVC   AUPKAM,BAGYMD             AGENCY/MEDIA                           
         MVC   AUPKDUE,SVDUEDT           BUY DUE DATE                           
         MVC   AUPKCLT,BCLT                                                     
         MVC   AUPKPRD,BPRD                                                     
         MVC   AUPKPRD2,BPRD2                                                   
         MVC   AUPKEST,BEST                                                     
         MVC   AUPKAUN,SVVERSN                                                  
         MVC   AUPKREV,SVREV                                                    
*                                                                               
         OI    DMINBTS,X'08'             PASS DELETES                           
         GOTO1 HIGH                                                             
         CLC   KEY(L'AUPKEY),KEYSAVE                                            
         BNE   VAUTH210                                                         
         TM    KEY+32,X'80'                                                     
         BNO   VAUTH220                  IF KEY IS MARKED FOR DELETION          
         NI    KEY+32,X'FF'-X'80'        UNMARK IT                              
         GOTO1 WRITE                                                            
         B     VAUTH220                                                         
*                                                                               
VAUTH210 XC    KEY,KEY                                                          
         MVC   KEY(L'AUPKEY),KEYSAVE     RESTORE KEY                            
         MVC   AUTKDA,DMDSKADD           PUT DISK ADDRESS IN KEY                
         GOTO1 ADD                       ADD PASSIVE KEY                        
*                                                                               
VAUTH220 LA    R6,KEY                                                           
         USING AUTRECD,R6                                                       
         XC    KEY,KEY                   ADD '0DC9' PASSIVE KEY                 
         MVC   AUSKTYP(2),=X'0DC9'                                              
         MVC   AUSKAM,BAGYMD             AGENCY/MEDIA                           
         MVC   AUSKSTDT,SVFLSDT          BUY FLIGHT START DATE                  
         MVC   AUSKCLT,BCLT                                                     
         MVC   AUSKPRD,BPRD                                                     
         MVC   AUSKPRD2,BPRD2                                                   
         MVC   AUSKEST,BEST                                                     
         MVC   AUSKAUN,SVVERSN                                                  
         MVC   AUSKREV,SVREV                                                    
*                                                                               
         OI    DMINBTS,X'08'             PASS DELETES                           
         GOTO1 HIGH                                                             
         CLC   KEY(L'AUPKEY),KEYSAVE                                            
         BNE   VAUTH230                                                         
         TM    KEY+32,X'80'                                                     
         BNO   VAUTH240                  IF KEY IS MARKED FOR DELETION          
         NI    KEY+32,X'FF'-X'80'        UNMARK IT                              
         GOTO1 WRITE                                                            
         B     VAUTH240                                                         
*                                                                               
VAUTH230 XC    KEY,KEY                                                          
         MVC   KEY(L'AUPKEY),KEYSAVE     RESTORE KEY                            
         MVC   AUTKDA,DMDSKADD           PUT DISK ADDRESS IN KEY                
         GOTO1 ADD                       ADD PASSIVE KEY                        
VAUTH240 DS    0H                                                               
*                                                                               
VAUTHX   DS    0H                                                               
*                                                                               
         CLC   =C'CCG',AUTPWD      ONLY ALLOW CCG TO CHANGE ENDDATE             
         BNE   VAUTHXX                                                          
         BRAS  RE,GETCKSM                                                       
         LA    R2,AUTPWDH                                                       
         CLC   SVCKSM,FULL                                                      
         BNE   ERRAUTH                                                          
VAUTHXX  B     XIT                                                              
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
MGTAB    DC    AL2(AUTFLTH-T217FFD),C'0'                                        
         DC    AL2(AUTHIAH-T217FFD),C'1'                                        
         DC    AL2(AUTDHTH-T217FFD),C'2'                                        
         DC    AL2(AUTOFLH-T217FFD),C'3'                                        
         DC    AL2(AUTCMSH-T217FFD),C'4'                                        
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
*        VALIDATE MARKET RECORD                                       *         
***********************************************************************         
VALMKT   NTR1                                                                   
*                                                                               
         LA    R2,AUTPWDH          VALIDATE THE PASSWORD FOR CHANGING           
         BAS   RE,VALPW                                                         
         BNE   ERRAUTH                                                          
*                                                                               
         USING AUTRECD,R6                                                       
         CLI   ACTEQU,ACTADD                                                    
         BE    VMKT01                                                           
         MVC   HALF(1),MINFRVNM    REVISION NUMBER                              
         MVI   ELCODE,X'01'                                                     
         GOTO1 REMELEM                                                          
         DROP  R6                                                               
*                                                                               
VMKT01   XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING MINFEL,R3                                                        
         MVI   MINFEL,MINFELQ      ELEMENT CODE                                 
         MVI   MINFLEN,MINFLNQ     LENGTH                                       
         CLI   ACTEQU,ACTADD                                                    
         BE    VMKT02                                                           
         MVC   MINFRVNM,HALF       REVISION NUMBER                              
*                                                                               
VMKT02   LA    R2,AUTREVH                                                       
         CLI   5(R2),0                                                          
         BE    VMKT02A                                                          
         MVI   MINFRVNM,X'99'      DEFAULT REVISION NUMBER                      
         CLI   AUTREN,C'0'                                                      
         BE    VMKT02A                                                          
*                                                                               
         TM    4(R2),X'08'         INPUT IS VALID NUMERIC?                      
         BNO   ERRINV                                                           
         ZIC   R5,5(R2)            INPUT LENGTH                                 
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,AUTREV(0)                                                    
*                                                                               
         MVC   HALF,DUB+6                                                       
         ZAP   WORK(2),=P'99'            FINDING 9'S COMPLIMENT                 
         SP    WORK(2),HALF              99 - (REV #) = 9'S COMP REV #          
         SRP   WORK(2),1,0               SHIFT LEFT 1 DIGIT                     
         MVC   MINFRVNM,WORK                                                    
*                                                                               
VMKT02A  LA    R2,AUTRDTH          MARKET ADD/REV DATE                          
         CLI   5(R2),0                                                          
         BE    VMKT03                                                           
         GOTO1 DATVAL,DMCB,(0,8(R2)),DATETEMP                                   
         OC    DMCB(4),DMCB                                                     
         BZ    ERRINV                                                           
         GOTO1 DATCON,DMCB,(0,DATETEMP),(3,MINFRVDT)                            
*                                                                               
VMKT03   LA    R2,AUTRNOH                                                       
         TM    4(R2),X'08'         INPUT IS VALID NUMERIC?                      
         BNO   ERRINV                                                           
*                                                                               
         ZIC   R5,5(R2)            INPUT LENGTH                                 
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,AUTRNO(0)                                                    
         CVB   RE,DUB                                                           
         STC   RE,MINFRVNO                                                      
*                                                                               
         LA    R2,AUTMDDH          MARKET DUE DATE                              
         TM    1(R2),X'20'         IS FIELD PROTECTED?                          
         BO    VMKT05              YES, THEN SKIP THIS                          
         CLI   5(R2),0                                                          
         BE    VMKT05                                                           
         GOTO1 DATVAL,DMCB,(0,8(R2)),DATETEMP                                   
         OC    DMCB(4),DMCB                                                     
         BZ    ERRINV                                                           
         GOTO1 DATCON,DMCB,(0,DATETEMP),(3,MINFDUDT)                            
*                                                                               
VMKT05   LA    R2,AUTGIDH          ORIGINAL GOAL INPUT FOR MKT                  
         CLI   5(R2),0                                                          
         BE    VMKT06                                                           
         GOTO1 DATVAL,DMCB,(0,8(R2)),DATETEMP                                   
         OC    DMCB(4),DMCB                                                     
         BZ    ERRINV                                                           
         GOTO1 DATCON,DMCB,(0,DATETEMP),(3,MINFGIDT)                            
*                                                                               
VMKT06   LA    R2,AUTGCDH          GOAL CHANGE DATE FOR MKT                     
         CLI   5(R2),0                                                          
         BE    VMKT07                                                           
         GOTO1 DATVAL,DMCB,(0,8(R2)),DATETEMP                                   
         OC    DMCB(4),DMCB                                                     
         BZ    ERRINV                                                           
         GOTO1 DATCON,DMCB,(0,DATETEMP),(3,MINFGCDT)                            
*                                                                               
VMKT07   LA    R2,AUTWRDH          FIRST WORK RECORD ADDED NWS ACTVTY           
         CLI   5(R2),0                                                          
         BE    VMKT10                                                           
         GOTO1 DATVAL,DMCB,(0,8(R2)),DATETEMP                                   
         OC    DMCB(4),DMCB                                                     
         BZ    ERRINV                                                           
         GOTO1 DATCON,DMCB,(0,DATETEMP),(3,MINFWKDT)                            
*                                                                               
VMKT10   LA    R2,AUTBTDH          LAST DATE BUYS TRANSFERRED FOR MKT           
         CLI   5(R2),0                                                          
         BE    VMKT15                                                           
         GOTO1 DATVAL,DMCB,(0,8(R2)),DATETEMP                                   
         OC    DMCB(4),DMCB                                                     
         BZ    ERRINV                                                           
         GOTO1 DATCON,DMCB,(0,DATETEMP),(3,MINFNXDT)                            
*                                                                               
VMKT15   LA    R2,AUTMLDH          LAST DATE ML REPORT RAN FOR MKT              
         CLI   5(R2),0                                                          
         BE    VMKT20                                                           
         GOTO1 DATVAL,DMCB,(0,8(R2)),DATETEMP                                   
         OC    DMCB(4),DMCB                                                     
         BZ    ERRINV                                                           
         GOTO1 DATCON,DMCB,(0,DATETEMP),(3,MINFMLDT)                            
*                                                                               
*                                                                               
VMKT20   LA    R2,AUTBGDH          LAST DATE BG REPORT RAN FOR MKT              
         CLI   5(R2),0                                                          
         BE    VMKT30                                                           
         GOTO1 DATVAL,DMCB,(0,8(R2)),DATETEMP                                   
         OC    DMCB(4),DMCB                                                     
         BZ    ERRINV                                                           
         GOTO1 DATCON,DMCB,(0,DATETEMP),(3,MINFBGDT)                            
*                                                                               
VMKT30   GOTO1 ADDELEM             ADDING "01" ELEM                             
         DROP  R3                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        VALIDATE STATION RECORD                                      *         
***********************************************************************         
VALSTA   NTR1                                                                   
*                                                                               
         LA    R2,AUTPWDH          VALIDATE THE PASSWORD FOR CHANGING           
         BAS   RE,VALPW                                                         
         BNE   ERRAUTH                                                          
*                                                                               
         USING AUTRECD,R6                                                       
         MVI   BYTE,0                                                           
         MVC   BYTE,SDTLFLG                                                     
         MVI   ELCODE,X'01'                                                     
         GOTO1 REMELEM                                                          
         DROP  R6                                                               
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING SDTLEL,R3                                                        
         MVI   SDTLEL,SDTLELQ      ELEMENT CODE                                 
         MVI   SDTLLEN,SDTLLENQ    LENGTH                                       
         MVC   SDTLFLG,BYTE                                                     
*                                                                               
         LA    R2,AUTOSDH          ORDER SENT DATE                              
         CLI   5(R2),0                                                          
         BE    VSTA10                                                           
         GOTO1 DATVAL,DMCB,(0,8(R2)),DATETEMP                                   
         OC    DMCB(4),DMCB                                                     
         BZ    ERRINV                                                           
         GOTO1 DATCON,DMCB,(0,DATETEMP),(3,SDTLORSN)                            
*                                                                               
VSTA10   LA    R2,AUTSCDH          STATION CONFIRMED DATE                       
         CLI   5(R2),0                                                          
         BE    VSTA20                                                           
         GOTO1 DATVAL,DMCB,(0,8(R2)),DATETEMP                                   
         OC    DMCB(4),DMCB                                                     
         BZ    ERRINV                                                           
         GOTO1 DATCON,DMCB,(0,DATETEMP),(3,SDTLSCNF)                            
         DROP  R3                                                               
*                                                                               
VSTA20   GOTO1 ADDELEM                                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE PASSWORD                             *         
* * * NOTE IN ORDER TO AVOID BAD RECORDS THIS PASSWORD IS FOR                   
* * * PROGRAMMERS ONLY, INSTEAD OF USING PFM. THERE IS A PASSWORD FOR           
* * * PRODUCT TO BE ABLE TO CHANGE DEMO CLIENTS ONLY!                           
***********************************************************************         
VALPW    NTR1                                                                   
*                                                                               
         CLC   =C'PWORD',AUTPWD    ANY AUTHORIZATION                            
         BE    VALPWX                                                           
*                                                                               
         CLC   =C'DEMO',AUTPWD     PASSWORD FOR DEMO CLIENTS ONLY               
         BNE   NO                                                               
         CLC   QCLT,=CL3'IRV'                                                   
         BE    VALPWX                                                           
         CLC   QCLT,=CL3'VFR'                                                   
         BE    VALPWX                                                           
         CLC   QCLT,=CL3'RCD'                                                   
         BE    VALPWX                                                           
         CLC   QCLT,=CL3'CHR'                                                   
         BE    VALPWX                                                           
         CLC   QCLT,=CL3'CHT'                                                   
         BNE   NO                                                               
*                                                                               
VALPWX   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
*                       DISPLAY RECORD                                *         
***********************************************************************         
DR       LA    R0,AUTSCDH          LAST FIELD                                   
         LA    R2,AUTAPPH          FIRST FIELD                                  
*                                                                               
DR10     ZIC   R1,0(R2)            LENGTH OF FIRST FIELD                        
         AHI   R1,-9               MINUS HEADER AND 1 FOR EX                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES      BLANK CURRENT FIELD                          
         OI    6(R2),X'80'         TRANSMIT                                     
DR20     ZIC   R1,0(R2)            R1 = LENGTH OF FIELD + HEADER                
         AR    R2,R1               NEXT FIELD                                   
         CR    R2,R0               END OF SCREEN?                               
         BH    DR30                                                             
         TM    1(R2),X'20'         NO, IS FIELD PROTECTED?                      
         BZ    DR10                NO, CLEAR IT                                 
         B     DR20                YES, BUMP TO NEXT FIELD                      
*                                                                               
DR30     BAS   RE,SSV              SET SYSTEM VALUES (XSPDIR AND XSPF)          
*                                                                               
         L     R6,AIO                                                           
         USING AUTRECD,R6                                                       
         OC    AUTKSTA,AUTKSTA                                                  
         BZ    DR40                                                             
         GOTO1 =A(DISSTA),RR=RELO  DISPLAY STATION RECORD                       
         B     DRX                                                              
DR40     OC    AUTKMKT,AUTKMKT                                                  
         BZ    DR50                                                             
         BAS   RE,DISMKT           DISPLAY MARKET RECORD                        
         B     DRX                                                              
DR50     GOTO1 =A(DISAUTH),RR=RELO DISPLAY AUTHORIZATION RECORD                 
*                                                                               
         DROP  R6                                                               
DRX      MVC   AIO,AIO1                                                         
         MVI   RDUPDATE,C'Y'                                                    
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        DISPLAY MARKET RECORD                                        *         
***********************************************************************         
         USING AUTRECD,R6                                                       
DISMKT   NTR1                                                                   
*                                                                               
         OC    MINFRVDT,MINFRVDT   MARKET ADD/REV DATE                          
         BZ    DMKT05                                                           
         GOTO1 DATCON,DMCB,(3,MINFRVDT),(10,AUTRDT)                             
         OI    AUTRDTH+6,X'80'     TRANSMIT                                     
         OI    AUTRDTH+4,X'20'     PREVIOUSLY VALIDATED                         
                                                                                
*                                                                               
DMKT05   OC    MINFRVNM,MINFRVNM   MARKET REV NUMBER                            
         BZ    DMKT10                                                           
         ZAP   WORK(2),=P'0'       REVISION NUMBER                              
         MVO   WORK(2),MINFRVNM    CONVERT TO PACKED                            
         SP    WORK(2),=P'99'                                                   
         EDIT  (P2,WORK),AUTREV,ZERO=NOBLANK,ALIGN=LEFT                         
*                                                                               
DMKT10   EDIT  (B1,MINFRVNO),AUTRNO,ZERO=NOBLANK,ALIGN=LEFT                     
*                                                                               
         OC    MINFDUDT,MINFDUDT   MARKET DUE DATE                              
         BZ    DMKT20                                                           
         GOTO1 DATCON,DMCB,(3,MINFDUDT),(10,AUTMDD)                             
         OI    AUTMDDH+6,X'80'     TRANSMIT                                     
         OI    AUTMDDH+4,X'20'     PREVIOUSLY VALIDATED                         
         OI    AUTMDDH+1,X'20'     MAKE PROTECTED                               
                                                                                
DMKT20   OC    MINFGIDT,MINFGIDT   GOAL INPUT DATE                              
         BZ    DMKT30                                                           
         GOTO1 DATCON,DMCB,(3,MINFGIDT),(10,AUTGID)                             
         OI    AUTGIDH+6,X'80'     TRANSMIT                                     
         OI    AUTGIDH+4,X'20'     PREVIOUSLY VALIDATED                         
*                                                                               
DMKT30   OC    MINFGCDT,MINFGCDT   GOAL CHANGE DATE                             
         BZ    DMKT35                                                           
         GOTO1 DATCON,DMCB,(3,MINFGCDT),(10,AUTGCD)                             
         OI    AUTGCDH+6,X'80'     TRANSMIT                                     
         OI    AUTGCDH+4,X'20'     PREVIOUSLY VALIDATED                         
*                                                                               
DMKT35   OC    MINFWKDT,MINFWKDT   WORK RECORD ADDED NWS ACTIVITY               
         BZ    DMKT40                                                           
         GOTO1 DATCON,DMCB,(3,MINFWKDT),(10,AUTWRD)                             
         OI    AUTWRDH+6,X'80'     TRANSMIT                                     
         OI    AUTWRDH+4,X'20'     PREVIOUSLY VALIDATED                         
*                                                                               
DMKT40   OC    MINFNXDT,MINFNXDT   NWS TRANSFER DATE                            
         BZ    DMKT50                                                           
         GOTO1 DATCON,DMCB,(3,MINFNXDT),(10,AUTBTD)                             
         OI    AUTBTDH+6,X'80'     TRANSMIT                                     
         OI    AUTBTDH+4,X'20'     PREVIOUSLY VALIDATED                         
*                                                                               
DMKT50   OC    MINFMLDT,MINFMLDT   LAST DATE ML REPORT RAN FOR MKT              
         BZ    DMKT60                                                           
         GOTO1 DATCON,DMCB,(3,MINFMLDT),(10,AUTMLD)                             
         OI    AUTMLDH+6,X'80'     TRANSMIT                                     
         OI    AUTMLDH+4,X'20'     PREVIOUSLY VALIDATED                         
*                                                                               
DMKT60   OC    MINFBGDT,MINFBGDT   LAST DATE BG REPORT RAN FOR MKT              
         BZ    DMKTX                                                            
         GOTO1 DATCON,DMCB,(3,MINFBGDT),(10,AUTBGD)                             
         OI    AUTBGDH+6,X'80'     TRANSMIT                                     
         OI    AUTBGDH+4,X'20'     PREVIOUSLY VALIDATED                         
DMKTX    B     XIT                                                              
         DROP  R6                                                               
*                                                                               
***********************************************************************         
*                       DISPLAY KEY                                   *         
***********************************************************************         
DK       DS    0X                                                               
         LA    R6,KEY                                                           
         USING AUTRECD,R6                                                       
         MVC   DSKADDR,AUTKDA                                                   
         L     R6,AIO                                                           
         BAS   RE,RSV              RESET SYSTEM VALUES                          
*                                                                               
         MVC   BYTE,AUTKAM         ISOLATE MEDIA CODE                           
         NI    BYTE,X'0F'                                                       
         LA    R5,MEDTAB           FIND MEDIA CODE USING MEDIA TABLE            
DK10     CLC   BYTE,1(R5)                                                       
         BE    DK20                                                             
         LA    R5,MEDTABLQ(R5)                                                  
         CLI   0(R5),X'FF'                                                      
         BNE   DK10                                                             
DK20     MVC   AUTMED,0(R5)                                                     
         LA    R2,AUTMEDH                                                       
         OI    6(R2),X'80'                                                      
         MVI   5(R2),1             TRANSMIT MEDIA CODE TO SCREEN                
         MVI   USEIONUM,2                                                       
         GOTO1 VALIMED                                                          
         MVC   AUTMDN,MEDNM        DISPLAY MEDIA NAME                           
         OI    AUTMDNH+6,X'80'                                                  
*                                                                               
         GOTO1 CLUNPK,DMCB,AUTKCLT,AUTCLT                                       
         OI    AUTCLTH+6,X'80'                                                  
         MVI   AUTCLTH+5,3                                                      
         LA    R2,AUTCLTH                                                       
         MVI   USEIONUM,2                                                       
         GOTO1 VALICLT                                                          
         MVC   AUTCLN,CLTNM        DISPLAY CLIENT NAME                          
         OI    AUTCLNH+6,X'80'                                                  
*                                                                               
         L     R2,AIO              FOLLOWING SEARCH CANNOT FAIL!!               
         USING CLTHDR,R2                                                        
         LA    R5,CLIST                                                         
DK50     CLC   AUTKPRD,3(R5)       COMPARE PRODUCT NUMBER                       
         BE    *+12                                                             
         LA    R5,4(R5)            BUMP TO NEXT PRODUCT                         
         B     DK50                                                             
         MVC   AUTPRD(3),0(R5)     GET PRODUCT MNEMONIC                         
         MVI   AUTPRDH+5,3         LENGTH IF NO PIGGYBACK                       
         CLI   AUTKPRD2,0                                                       
         BE    DK58                                                             
*                                                                               
         LA    R5,CLIST                                                         
DK55     CLC   AUTKPRD2,3(R5)      COMPARE PIGGYBACK PRODUCT NUMBER             
         BE    *+12                                                             
         LA    R5,4(R5)            BUMP TO NEXT PRODUCT                         
         B     DK55                                                             
         MVI   AUTPRD+3,C'-'                                                    
         MVC   AUTPRD+4(3),0(R5)   GET PRODUCT MNEMONIC                         
         MVI   AUTPRDH+5,7                                                      
         DROP  R2                                                               
*                                                                               
DK58     OC    AUTPRD,SPACES                                                    
         OI    6(R2),X'80'         TRANSMIT PRODUCT                             
         LA    R2,TEMPFLDH         USE FAKE FIELD FOR VALIPRD                   
         MVI   5(R2),3                                                          
         MVC   8(3,R2),AUTPRD                                                   
         MVI   USEIONUM,2                                                       
         GOTO1 VALIPRD                                                          
         MVC   AUTPDN,PRDNM        DISPLAY PRODUCT NAME                         
         OI    AUTPDNH+6,X'80'                                                  
*                                                                               
         EDIT  AUTKEST,AUTEST,ALIGN=LEFT                                        
         OI    AUTESTH+6,X'80'     ESTIMATE                                     
         MVI   AUTESTH+4,X'08'     VALID NUMERIC                                
         STC   R0,AUTESTH+5                                                     
*                                                                               
DK60     LA    R2,AUTESTH                                                       
         MVI   USEIONUM,2                                                       
         GOTO1 VALIEST                                                          
         L     R2,AIO                                                           
         USING ESTHDR,R2                                                        
         GOTO1 DATCON,DMCB,(0,ESTART),(10,AUTESD)                               
         GOTO1 DATCON,DMCB,(0,EEND),(10,AUTESD+11)                              
         MVI   AUTESD+9,C'-'                                                    
         OI    AUTESDH+6,X'80'                                                  
         DROP  R2                                                               
*                                                                               
DK70     OC    AUTKMKT,AUTKMKT     MARKET/STATION RECORD?                       
         BZ    DK90                                                             
         MVC   BMKT,AUTKMKT                                                     
         MVC   BSTA,AUTKSTA                                                     
         XC    WORK,WORK           CLEAR WORK                                   
         GOTO1 MSUNPK,DMCB,(X'80',BMKTSTA),AUTMKT,WORK                          
         CLI   WORK+5,X'40'        HAVE A NETWORK?                              
         BNH   *+8                 NO                                           
         MVI   WORK+4,C'/'         YES - MOVE IN A "/" BEFORE NETWORK           
*                                                                               
         OC    AUTKSTA,AUTKSTA                                                  
         BNZ   DK80                                                             
         XC    AUTSTA,AUTSTA       CLEAR STATION                                
         MVI   AUTSTAH+5,0         NO INPUT                                     
         OI    AUTSTAH+6,X'80'     TRANSMIT                                     
         LA    R2,AUTMKTH          AUTH MARKET RECORD                           
         MVI   5(R2),4                                                          
         OI    AUTMKTH+4,X'08'     NUMERIC                                      
         MVI   USEIONUM,2                                                       
         GOTO1 VALIMKT                                                          
         OI    6(R2),X'80'                                                      
         MVC   AUTMKN,MKTNM                                                     
         OI    AUTMKNH+6,X'80'                                                  
         B     DK90                                                             
*                                                                               
DK80     MVC   AUTSTA,WORK         AUTH STATION RECORD                          
         LA    R2,AUTSTAH                                                       
         MVI   5(R2),5                                                          
         MVI   USEIONUM,2                                                       
         GOTO1 VALISTA                                                          
         OI    6(R2),X'80'                                                      
         OI    AUTMKTH+6,X'80'                                                  
         MVC   AUTMKN,MKTNM                                                     
         OI    AUTMKNH+6,X'80'                                                  
*                                                                               
DK90     ZAP   WORK(2),=P'0'       VERSION NUMBER                               
         MVO   WORK(2),AUTKAUN     CONVERT TO PACKED                            
         ZAP   WORK+2(2),=P'99'                                                 
         SP    WORK+2(2),WORK(2)                                                
         ZAP   WORK(2),WORK+2(2)   WORK(2)=PACKED VERS NUMBER                   
         BAS   RE,GETALPH                                                       
         MVC   AUTVER,BYTE                                                      
         OI    AUTVERH+6,X'80'                                                  
*                                                                               
         OC    AUTKMKT,AUTKMKT     NO REVISION IF MKT                           
         BNZ   DK100                                                            
         ZAP   WORK(2),=P'0'       REVISION NUMBER                              
         MVO   WORK(2),AUTKREV     CONVERT TO PACKED                            
         SP    WORK(2),=P'99'                                                   
         EDIT  (P2,WORK),AUTREN,ZERO=NOBLANK,ALIGN=LEFT                         
*                                                                               
DK100    MVC   AUTDKA(3),=C'DA='   DISPLAY DISK ADDRESS                         
         GOTO1 HEXOUT,DMCB,DSKADDR,AUTDKA+3,4                                   
         OI    AUTDKAH+6,X'80'     TRANSMIT                                     
*                                                                               
         BAS   RE,SSV              SET SYSTEM VALUES                            
         MVC   AIO,AIO1                                                         
         DROP  R6                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        LISTRECS                                                     *         
***********************************************************************         
LR       MVI   NLISTS,14           LIST 14 RECORDS PER SCREEN                   
         LA    R4,KEY                                                           
KEYRD    USING AUTRECD,R4                                                       
         LA    R3,AUTHKEY                                                       
FILTER   USING AUTRECD,R3                                                       
         OC    KEY(AUTRLEN-AUTKEY),KEY   FIRST TIME?                            
         BNZ   LR10                                                             
         MVC   KEY,AUTHKEY         RESTORE KEY                                  
         BAS   RE,CLRKEY           CLEAR BLANK KEY FIELDS                       
*                                                                               
LR10     GOTO1 HIGH                                                             
*                                                                               
LR20     CLC   KEY(AUTKCLT-AUTKEY),AUTHKEY SAME RECORD TYPE AND MEDIA?          
         BNE   LRX                                                              
*                                                                               
         OC    FILTER.AUTKCLT,FILTER.AUTKCLT  CLIENT FILTER?                    
         BZ    LR30                                                             
         CLC   KEYRD.AUTKCLT,FILTER.AUTKCLT                                     
         BNE   LR110                                                            
*                                                                               
LR30     OC    FILTER.AUTKPRD,FILTER.AUTKPRD  PRODUCT FILTER?                   
         BZ    LR40                                                             
         CLC   KEYRD.AUTKPRD,FILTER.AUTKPRD                                     
         BNE   LR110                                                            
*                                                                               
LR40     OC    FILTER.AUTKEST,FILTER.AUTKEST  ESTIMATE FILTER?                  
         BZ    LR50                                                             
         CLC   KEYRD.AUTKEST,FILTER.AUTKEST                                     
         BNE   LR110                                                            
*                                                                               
LR50     OC    FILTER.AUTKAUN,FILTER.AUTKAUN  VERSION FILTER?                   
         BZ    LR51                                                             
         CLC   KEYRD.AUTKAUN,FILTER.AUTKAUN                                     
         BNE   LR110                                                            
*                                                                               
LR51     TM    FLAG1,FL1NOMKT            DON'T DISPLAY MARKETS?                 
         BNO   LR52                                                             
         OC    KEYRD.AUTKMKT,KEYRD.AUTKMKT                                      
         BNZ   LR110                                                            
*                                                                               
LR52     OC    FILTER.AUTKMKT,FILTER.AUTKMKT  MARKET FILTER?                    
         BZ    LR54                                                             
         CLC   KEYRD.AUTKMKT,FILTER.AUTKMKT                                     
         BNE   LR110                                                            
*                                                                               
LR54     TM    FLAG1,FL1NOSTA            DON'T DISPLAY STATIONS?                
         BNO   LR55                                                             
         OC    KEYRD.AUTKSTA,KEYRD.AUTKSTA                                      
         BNZ   LR110                                                            
*                                                                               
LR55     OC    FILTER.AUTKSTA,FILTER.AUTKSTA  STATION FILTER?                   
         BZ    LR56                                                             
         CLC   KEYRD.AUTKSTA,FILTER.AUTKSTA                                     
         BNE   LR110                                                            
*                                                                               
LR56     OC    FILTER.AUTKREV,FILTER.AUTKREV  REVISION FILTER?                  
         BZ    LR60                                                             
         CLC   KEYRD.AUTKREV,FILTER.AUTKREV                                     
         BNE   LR110                                                            
*                                                                               
LR60     XC    AUTCLN,AUTCLN       CLEAR CLIENT, PRD, EST, AND MKT              
         OI    AUTCLNH+6,X'80'     NAMES                                        
LR63     XC    AUTPDN,AUTPDN                                                    
         OI    AUTPDNH+6,X'80'                                                  
LR65     XC    AUTESD,AUTESD                                                    
         OI    AUTESDH+6,X'80'                                                  
LR67     XC    AUTMKN,AUTMKN                                                    
         OI    AUTMKNH+6,X'80'                                                  
*                                                                               
LR70     MVC   LISTAR,SPACES                                                    
         MVC   LSMED,QMED                                                       
         GOTO1 CLUNPK,DMCB,KEYRD.AUTKCLT,LSCLT                                  
*                                                                               
         MVC   LSTKEY,KEY          SAVE KEY                                     
         BAS   RE,RSV              RESET SYSTEM VALUES                          
         LA    R4,LSTKEY                                                        
         MVC   CLTB,KEYRD.AUTKCLT                                               
         MVC   PRDB,KEYRD.AUTKPRD  GET PRODUCT MNEMONIC                         
         BAS   RE,PRDT                                                          
         MVC   LSPRD,PRD                                                        
         CLI   KEYRD.AUTKPRD2,0                                                 
         BE    LR80                                                             
         MVC   PRDB,KEYRD.AUTKPRD2 PIGGYBACK PRODUCT?                           
         BAS   RE,PRDT                                                          
         MVC   LSPRD2,PRD                                                       
         MVI   LSDASH,C'-'                                                      
LR80     MVC   KEY,LSTKEY          RESTORE KEY                                  
         LA    R4,KEY                                                           
*                                                                               
         EDIT  (B1,KEYRD.AUTKEST),(3,LSEST),FILL=0 ESTIMATE                     
         ZAP   WORK(2),=P'0'       VERSION                                      
         MVO   WORK(2),KEYRD.AUTKAUN CONVERT TO PACKED                          
         ZAP   WORK+2(2),=P'99'                                                 
         SP    WORK+2(2),WORK(2)                                                
         ZAP   WORK(2),WORK+2(2)   WORK(2)=PACKED VERS NUMBER                   
         CP    WORK(2),=P'0'       ORIGINAL, DON'T PRINT                        
         BE    LR85                                                             
*                                                                               
         BAS   RE,GETALPH          GET ALPHABETIC VERSION CODE                  
         MVC   LSVER,BYTE                                                       
         MVI   LSDASH2,C'-'                                                     
*                                                                               
LR85     OC    KEYRD.AUTKMKT,KEYRD.AUTKMKT                                      
         BZ    LR90                                                             
         GOTO1 MSUNPK,DMCB,(X'80',KEYRD.AUTKMKT),LSMKT,LSSTA  MKT/STA           
         CLC   =C'AAAAA',LSSTA                                                  
         BNE   LR95                                                             
         XC    LSSTA,LSSTA                                                      
         B     LR95                                                             
*                                                                               
LR90     ZAP   WORK(2),=P'0'       REVISION NUMBER                              
         MVO   WORK(2),KEYRD.AUTKREV CONVERT TO PACKED                          
         SP    WORK(2),=P'99'                                                   
         EDIT  (P2,WORK),LSREV,ZERO=NOBLANK,ALIGN=LEFT                          
*                                                                               
LR95     BAS   RE,SSV              SET SYSTEM VALUES (XSPDIR AND XSPF)          
         MVC   AIO,AIO1            RESTORE RECORD                               
         GOTO1 HIGH                                                             
         CLC   LSTKEY,KEY                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC              NEEDED TO BUILD CORRECT DISK                 
         CLI   DMCB+8,0            ADDRESSES IN LISTMON TABLE                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    KEYRD.AUTKSTA,KEYRD.AUTKSTA STATION REC - NO DUE DATE            
         BNZ   LR100                                                            
         L     R4,AIO                                                           
         LA    R5,KEYRD.AUDDUEDT         AUTHORIZATION RECORD                   
         OC    KEYRD.AUTKMKT,KEYRD.AUTKMKT                                      
         BZ    *+8                                                              
         LA    R5,KEYRD.MINFDUDT         MARKET AUTHORIZATION RECORD            
         GOTO1 DATCON,DMCB,(3,(R5)),(5,LSDUE)                                   
LR100    GOTO1 LISTMON             PRINT LINE                                   
*                                                                               
LR110    GOTO1 SEQ                                                              
         LA    R4,KEY                                                           
         B     LR20                                                             
*                                                                               
         DROP  KEYRD,FILTER                                                     
LRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
***********************************************************************         
* CONVERT BINARY PRODUCT CODE TO NAME OF PRODUCT                                
PRDT     NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CKEY,R4                                                          
         MVI   CKEYTYPE,0          RECORD TYPE                                  
         MVC   CKEYAM,BAGYMD       MEDIA                                        
         MVC   CKEYCLT,CLTB        CLIENT                                       
         DROP  R4                                                               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'CKEY),KEYSAVE SAME CLIENT?                                 
         BE    PRDT10                                                           
         DC    H'0'                                                             
                                                                                
PRDT10   MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING CKEY,R6                                                          
         B     *+8                                                              
                                                                                
PRDT20   LA    R6,4(R6)                                                         
         CLI   CLIST+3,0                                                        
         BNE   *+6                                                              
         DC    H'0'                PRODUCT CODE DOESN'T EXIST                   
         CLC   CLIST+3(L'PRDB),PRDB                                             
         BNE   PRDT20                                                           
                                                                                
         MVC   PRD,CLIST           PRODUCT NAME                                 
         DROP  R6                                                               
                                                                                
PRDTX    MVC   AIO,AIO1            RESTORE I/O AREA ADDRESS                     
         B     XIT                                                              
                                                                                
         EJECT                                                                  
*                                                                               
********************************************************************            
*        GET ALPHABETIC VERSION CODE                               *            
********************************************************************            
GETALPH  NTR1                                                                   
         LA    R3,VERSTAB                                                       
GALPH10  CLI   0(R3),C' '                                                       
         BNE   *+6                                                              
         DC    H'00'                                                            
         CP    WORK(2),=P'0'                                                    
         BNH   GALPH20                                                          
         SP    WORK(2),=P'1'                                                    
         LA    R3,1(R3)                                                         
         B     GALPH10                                                          
GALPH20  MVC   BYTE,0(R3)                                                       
         B     XIT                                                              
*                                                                               
********************************************************************            
*        CLEAR BLANK KEY FIELDS                                    *            
********************************************************************            
         USING AUTRECD,R4                                                       
CLRKEY   NTR1                                                                   
         CLI   AUTRENH+5,0                                                      
         BNE   CLK05                                                            
         MVI   AUTKREV,0                                                        
CLK05    CLI   AUTSTAH+5,0                                                      
         BNE   CLK10                                                            
         XC    AUTKSTA,AUTKSTA                                                  
CLK10    CLI   AUTMKTH+5,0                                                      
         BNE   CLK15                                                            
         XC    AUTKMKT,AUTKMKT                                                  
CLK15    CLI   AUTVERH+5,0                                                      
         BNE   CLK20                                                            
         MVI   AUTKAUN,0                                                        
CLK20    CLI   AUTESTH+5,0                                                      
         BNE   CLK25                                                            
         MVI   AUTKEST,0                                                        
CLK25    CLI   AUTPRDH+5,0                                                      
         BNE   CLK30                                                            
         XC    AUTKPRD(2),AUTKPRD                                               
CLK30    CLI   AUTCLTH+5,0                                                      
         BNE   CLKX                                                             
         XC    AUTKCLT,AUTKCLT                                                  
CLKX     B     XIT                                                              
         DROP  R4                                                               
********************************************************************            
*                     DELETE RECORDS                               *            
********************************************************************            
*                                                                               
DEL      GOTO1 DATCON,DMCB,(5,0),(0,WORK)                                       
         GOTO1 GETDAY,DMCB,WORK,WORK+6                                          
         CLC   WORK+6(3),SPACES    TEST VALID DAY OF WEEK                       
         BNE   *+6                 YES                                          
         DC    H'0'                                                             
         CLC   WORK+6(3),=C'FRI'                                                
         BNE   ERRNTFR                                                          
*                                                                               
         USING AUTRECD,R6                                                       
         L     R6,AIO                                                           
         NI    DMINBTS,X'FF'-X'08' DON'T READ FOR DELETES                       
         OC    AUTKSTA,AUTKSTA     IF STATION AUTH RECORD, EXIT                 
         BNZ   DELX                                                             
         OC    AUTMKT,AUTMKT       IF MARKET RECORD, CHECK FOR                  
         BZ    DEL70               STATION RECORDS                              
*                                                                               
DEL10    GOTO1 SEQ                                                              
         CLC   KEY(AUTKSTA-AUTRECD),AUTHKEY     SAME MARKET??                   
         BNE   DEL95                                                            
         LA    R2,AUTMKTH          CAN'T DELETE MKT RECORD IF                   
         B     ERRDMKT             STATION RECORD EXISTS                        
*                                                                               
*DELETING AUTHORIZATION RECORDS                                                 
*                                                                               
DEL70    MVC   SVDUEDT,AUDDUEDT    DUE DATE                                     
         MVC   SVFLSDT,AUDFLST     BUY FLIGHT START DATE                        
         LA    R6,KEY                                                           
         MVC   KEY,AUTHKEY                                                      
         MVI   AUTKREV,0                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(AUTKMKT-AUTRECD),AUTHKEY                                     
         BE    *+6                                                              
         DC    H'00'                                                            
         LA    R2,AUTRENH                                                       
         CLC   AUTKREV,SVREV                                                    
         BNE   ERRCRNT             NOT CURRENT REVISION                         
*                                                                               
         CLI   SVREV,X'99'         IF ORIGINAL REVISION, DELETE                 
         BNE   DEL90               ALL MARKET AND STATION RECORDS               
DEL80    GOTO1 SEQ                                                              
         CLC   KEY(AUTKMKT-AUTRECD),AUTHKEY                                     
         BNE   DEL90                                                            
         OC    AUTKMKT,AUTKMKT     MARKET/STATION RECORD?                       
         BZ    DEL90                                                            
         MVI   KEY+32,X'80'        MARK KEY FOR DELETION                        
         GOTO1 WRITE                AND OVERWRITE ALL OTHER BITS                
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   AUTRSTAT,X'80'      MARK RECORD FOR DELETION                     
         GOTO1 PUTREC               AND OVERWRITE ALL OTHER BITS                
         B     DEL80                                                            
*                                                                               
DEL90    LA    R6,KEY              DELETE PASSIVE KEY                           
         XC    KEY,KEY                                                          
         MVC   AUPKTYP(2),=X'0DB9'                                              
         MVC   AUPKAM,BAGYMD                                                    
         MVC   AUPKDUE,SVDUEDT                                                  
         MVC   AUPKCLT,BCLT                                                     
         MVC   AUPKPRD,BPRD                                                     
         MVC   AUPKPRD2,BPRD2                                                   
         MVC   AUPKEST,BEST                                                     
         MVC   AUPKAUN,SVVERSN                                                  
         MVC   AUPKREV,SVREV                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'AUPKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'00'                                                            
         MVI   KEY+32,X'80'        MARK FOR DELETION                            
         GOTO1 WRITE                AND OVERWRITE ALL OTHER BITS                
*                                                                               
         LA    R6,KEY              DELETE PASSIVE KEY                           
         XC    KEY,KEY                                                          
         MVC   AUSKTYP(2),=X'0DC9'                                              
         MVC   AUSKAM,BAGYMD                                                    
         MVC   AUSKSTDT,SVFLSDT                                                 
         MVC   AUSKCLT,BCLT                                                     
         MVC   AUSKPRD,BPRD                                                     
         MVC   AUSKPRD2,BPRD2                                                   
         MVC   AUSKEST,BEST                                                     
         MVC   AUSKAUN,SVVERSN                                                  
         MVC   AUSKREV,SVREV                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'AUPKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'00'                                                            
         MVI   KEY+32,X'80'        MARK FOR DELETION                            
         GOTO1 WRITE                AND OVERWRITE ALL OTHER BITS                
*                                                                               
DEL95    MVC   KEY(L'AUTHKEY),AUTHKEY  RESTORE ORIGINAL KEY                     
         GOTO1 HIGH                    AND RECORD INTO AIO                      
         CLC   KEY(L'AUTHKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'00'                                                            
         NI    KEY+32,X'FF'-X'40'  CLEAR X'40' BIT                              
         GOTO1 WRITE                                                            
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         NI    AUTRSTAT,X'FF'-X'40'  CLEAR X'40' BIT                            
         GOTO1 PUTREC                                                           
*                                                                               
DELX     B     XIT                                                              
********************************************************************            
*                     RESTORE RECORDS                              *            
********************************************************************            
*                                                                               
         USING AUTRECD,R6                                                       
RES      L     R6,AIO                                                           
         OC    AUTKMKT,AUTKMKT     IF MKT/STA AUTH RECORD, EXIT                 
         BNZ   RESX                                                             
         MVC   SVDUEDT,AUDDUEDT    DUE DATE                                     
         MVC   SVFLSDT,AUDFLST     BUY FLIGHT START DATE                        
*                                                                               
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   AUPKTYP(2),=X'0DB9'                                              
         MVC   AUPKAM,BAGYMD                                                    
         MVC   AUPKDUE,SVDUEDT                                                  
         MVC   AUPKCLT,BCLT                                                     
         MVC   AUPKPRD,BPRD                                                     
         MVC   AUPKPRD2,BPRD2                                                   
         MVC   AUPKEST,BEST                                                     
         MVC   AUPKAUN,SVVERSN                                                  
         MVC   AUPKREV,SVREV                                                    
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         CLC   KEY(L'AUPKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'00'                                                            
         TM    KEY+32,X'80'        IF KEY IS MARKED FOR DELETION                
         BNO   RES10               UNMARK IT                                    
         NI    KEY+32,X'FF'-X'80'                                               
         GOTO1 WRITE                                                            
*                                                                               
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   AUSKTYP(2),=X'0DC9'                                              
         MVC   AUSKAM,BAGYMD                                                    
         MVC   AUSKSTDT,SVFLSDT                                                 
         MVC   AUSKCLT,BCLT                                                     
         MVC   AUSKPRD,BPRD                                                     
         MVC   AUSKPRD2,BPRD2                                                   
         MVC   AUSKEST,BEST                                                     
         MVC   AUSKAUN,SVVERSN                                                  
         MVC   AUSKREV,SVREV                                                    
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         CLC   KEY(L'AUPKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'00'                                                            
         TM    KEY+32,X'80'        IF KEY IS MARKED FOR DELETION                
         BNO   RES10               UNMARK IT                                    
         NI    KEY+32,X'FF'-X'80'                                               
         GOTO1 WRITE                                                            
*                                                                               
RES10    MVC   KEY(L'AUTHKEY),AUTHKEY  RESTORE ORIGINAL KEY                     
         GOTO1 HIGH                    AND RECORD INTO AIO                      
         CLC   KEY(L'AUTHKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 GETREC                                                           
RESX     B     XIT                                                              
********************************************************************            
*                     SET SYSTEM VALUES                            *            
********************************************************************            
                                                                                
SSV      NTR1                                                                   
         MVC   LKEY,=H'32'             DETAILS OF DIRECTORY AND KEY             
         MVC   LSTATUS,=H'4'                                                    
         MVC   DATADISP,=H'42'                                                  
         MVC   SYSFIL,=C'XSPFIL  '                                              
         MVC   SYSDIR,=C'XSPDIR  '                                              
         XIT1                                                                   
                                                                                
********************************************************************            
*                   RESET SYSTEM VALUES                            *            
********************************************************************            
                                                                                
RSV      NTR1                                                                   
         MVC   LKEY,=H'13'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'     USUALLY SPOTFILE                             
         MVC   SYSFIL,=C'SPTFIL  '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
         XIT1                                                                   
                                                                                
***********************************************************************         
*                          ERROR MESSAGES                             *         
***********************************************************************         
ERRINV   MVI   ERROR,INVALID                                                    
         B     VSFMERR                                                          
ERRMIS   MVI   ERROR,MISSING                                                    
         B     VSFMERR                                                          
ERRNTFR  MVC   ERRNUM,=AL2(NOTFRIDY)                                            
         B     SPERREX                                                          
ERRBKLN  MVC   ERRNUM,=AL2(BKLNINV)                                             
         B     SPERREX                                                          
ERRAUTH  MVC   ERRNUM,=AL2(NOTAUTH)                                             
         B     SPERREX                                                          
ERRCRNT  MVC   ERRNUM,=AL2(CRNTREV)                                             
         B     SPERREX                                                          
ERRDMKT  MVC   ERRNUM,=AL2(NODELMKT)                                            
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
NOTAUTH  EQU   175                 NOT AUTHORIZED FOR THIS FUNCTION             
BKLNINV  EQU   449                 BREAK LN MUST BE 1-4                         
CRNTREV  EQU   803                 CAN ONLY DELETE CURRENT REVISION             
NODELMKT EQU   804                 STA RECORDS EXIST UNDER THIS MKT             
NOTFRIDY EQU   1378                NOT FRIDAY, CAN'T DELETE                     
*                                                                               
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        SETUP                                                        *         
***********************************************************************         
SETUP    NTR1                                                                   
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
         MVI   ACTELOPT,C'N'       DON'T HANDLE ACTIVITY ELEMENTS               
         CLI   T217FFD+1,C'*'      DDS TERMINAL?                                
         BNE   ERRAUTH             IF NO, NOT AUTHORIZED                        
SETUPX   B     XIT                                                              
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        TABLE FOR MEDIA AND ELEMENT CODES                            *         
***********************************************************************         
MEDTAB   DS    0X                                                               
         DC    CL1'T',XL1'01'                                                   
MEDTABLQ EQU   *-MEDTAB                                                         
         DC    CL1'R',XL1'02'                                                   
         DC    CL1'N',XL1'03'                                                   
         DC    CL1'X',XL1'04'                                                   
         DC    CL1'C',XL1'08'                                                   
         DC    X'FF'                                                            
*                                                                               
VERSTAB  DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ '                                   
***********************************************************************         
*        LTORG                                                        *         
***********************************************************************         
         LTORG                                                                  
*                                                                               
         DROP  RB,R7                                                            
         EJECT                                                                  
*                                                                               
******************                                                              
* SPECIAL VERSION OF CHECKSUM, WHICH ONLY ALLOWS CHANGE OF ENDDDATE             
******************                                                              
GETCKSM  NTR1  BASE=*,LABEL=*                                                   
         L     R2,AIO              COPY RECORD TO AIO3                          
         LH    R3,AUTRLEN-AUTRECD(R2)                                           
         L     RE,AIO3                                                          
         LR    RF,R3                                                            
         MVCL  RE,R2                                                            
*                                                                               
         L     R6,AIO3                                                          
         MVI   ELCODE,X'01'        PRIMARY DATE ELEMENT                         
         BRAS  RE,GETEL                                                         
         USING AUDEL,R6                                                         
         XC    AUDFLEN,AUDFLEN     CLEAR FLIGHT END DATE                        
*                                                                               
         MVI   ELCODE,X'01'        PRIMARY DATE ELEMENT                         
         BRAS  RE,GETEL                                                         
         USING AUDEL,R6                                                         
         XC    AUDFLEN,AUDFLEN     CLEAR FLIGHT END DATE                        
*                                                                               
         L     R2,AIO3             R2 = A(RECORD)                               
         SR    R3,R3                                                            
         ICM   R3,3,32(R2)                                                      
         SR    R0,R0                                                            
         CKSM  R0,R2               CALCULATE CHECK SUM                          
         JO    *-4                                                              
         ST    R0,FULL                                                          
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        DISPLAY STATION RECORD                                       *         
***********************************************************************         
         USING AUTRECD,R6                                                       
DISSTA   NTR1  BASE=*                                                           
         OC    SDTLORSN,SDTLORSN   ORDER SENT DATE                              
         BZ    DSTA10                                                           
         GOTO1 DATCON,DMCB,(3,SDTLORSN),(10,AUTOSD)                             
         OI    AUTOSDH+6,X'80'     TRANSMIT                                     
         OI    AUTOSDH+4,X'20'     PREVIOUSLY VALIDATED                         
*                                                                               
DSTA10   OC    SDTLSCNF,SDTLSCNF   STATION CONFIRMED DATE                       
         BZ    DSTA20                                                           
         GOTO1 DATCON,DMCB,(3,SDTLSCNF),(10,AUTSCD)                             
         OI    AUTSCDH+6,X'80'     TRANSMIT                                     
         OI    AUTSCDH+4,X'20'     PREVIOUSLY VALIDATED                         
*                                                                               
DSTA20   XIT1                                                                   
         LTORG                                                                  
         DROP  R6                                                               
*                                                                               
***********************************************************************         
*        DISPLAY AUTHORIZATION RECORD                                 *         
***********************************************************************         
         USING AUTRECD,R6                                                       
DISAUTH  NTR1  BASE=*                                                           
*                                                                               
         OC    AUDISDT,AUDISDT     BUY ISSUE DATE                               
         BZ    DAUTH10                                                          
         GOTO1 DATCON,DMCB,(3,AUDISDT),(10,AUTBIS)                              
         OI    AUTBISH+6,X'80'     TRANSMIT                                     
         OI    AUTBISH+4,X'20'     PREVIOUSLY VALIDATED                         
                                                                                
DAUTH10  OC    AUDFLST,AUDFLST     BUY FLIGHT START DATE                        
         BZ    DAUTH20                                                          
         GOTO1 DATCON,DMCB,(3,AUDFLST),(10,AUTBFS)                              
         OI    AUTBFSH+6,X'80'     TRANSMIT                                     
         OI    AUTBFSH+4,X'20'     PREVIOUSLY VALIDATED                         
         OI    AUTBFSH+1,X'20'     MAKE PROTECTED                               
                                                                                
DAUTH20  OC    AUDFLEN,AUDFLEN     BUY FLIGHT END DATE                          
         BZ    DAUTH30                                                          
         GOTO1 DATCON,DMCB,(3,AUDFLEN),(10,AUTBFE)                              
         OI    AUTBFEH+6,X'80'     TRANSMIT                                     
         OI    AUTBFEH+4,X'20'     PREVIOUSLY VALIDATED                         
                                                                                
DAUTH30  OC    AUDDUEDT,AUDDUEDT   BUY DUE DATE                                 
         BZ    DAUTH40                                                          
         GOTO1 DATCON,DMCB,(3,AUDDUEDT),(10,AUTBDU)                             
         OI    AUTBDUH+6,X'80'     TRANSMIT                                     
         OI    AUTBDUH+4,X'20'     PREVIOUSLY VALIDATED                         
         OI    AUTBDUH+1,X'20'     MAKE PROTECTED                               
                                                                                
DAUTH40  OC    AUDRVDT,AUDRVDT     APPROVAL DATE                                
         BZ    DAUTH50                                                          
         GOTO1 DATCON,DMCB,(3,AUDRVDT),(10,AUTRDA)                              
         OI    AUTRDAH+6,X'80'     TRANSMIT                                     
         OI    AUTRDAH+4,X'20'     PREVIOUSLY VALIDATED                         
         DROP  R6                                                               
                                                                                
DAUTH50  MVI   ELCODE,X'02'        SECONDARY DATE ELEMENT                       
         BRAS  RE,GETEL                                                         
         BNE   DAUTH90             REQUIRED ELEMENT                             
         USING A2DELD,R6                                                        
                                                                                
         OC    A2DPRPOS,A2DPRPOS   PRE-POST DATE                                
         BZ    DAUTH70                                                          
         GOTO1 DATCON,DMCB,(3,A2DPRPOS),(10,AUTPPR)                             
         OI    AUTPPRH+6,X'80'     TRANSMIT                                     
         OI    AUTPPRH+4,X'20'     PREVIOUSLY VALIDATED                         
                                                                                
DAUTH70  OC    A2DTIME,A2DTIME     TIME PERIOD POST DUE DATE                    
         BZ    DAUTH80                                                          
         GOTO1 DATCON,DMCB,(3,A2DTIME),(10,AUTTPP)                              
         OI    AUTTPPH+6,X'80'     TRANSMIT                                     
         OI    AUTTPPH+4,X'20'     PREVIOUSLY VALIDATED                         
                                                                                
DAUTH80  OC    A2DAFFID,A2DAFFID   AFFIDAVIT DATE                               
         BZ    DAUTH90                                                          
         GOTO1 DATCON,DMCB,(3,A2DAFFID),(10,AUTAFF)                             
         OI    AUTAFFH+6,X'80'     TRANSMIT                                     
         OI    AUTAFFH+4,X'20'     PREVIOUSLY VALIDATED                         
         DROP  R6                                                               
*                                                                               
DAUTH90  L     R6,AIO                                                           
         MVI   ELCODE,X'04'        INFORMATION ELEMENT                          
         BRAS  RE,GETEL                                                         
         BE    *+6                 REQUIRED ELEMENT                             
         DC    H'0'                                                             
*                                                                               
         USING AINFELD,R6                                                       
         MVC   AUTAPP,AINFSPRQ                                                  
         OI    AUTAPPH+6,X'80'     DISPLAY SUPERVISOR APPROVAL REQUIRED         
         OI    AUTAPPH+4,X'20'     PREVIOUSLY VALIDATED                         
*                                                                               
         MVI   AUTBBA,C'G'         0=GOAL,1=DOLLARS                             
         CLI   AINFBYBS,C'0'                                                    
         BE    DAUTH95                                                          
         MVI   AUTBBA,C'D'                                                      
         CLI   AINFBYBS,C'1'                                                    
         BE    *+6                                                              
         DC    H'00'                                                            
DAUTH95  OI    AUTBBAH+6,X'80'     DISPLAY BUY BASIS                            
         OI    AUTBBAH+4,X'20'     PREVIOUSLY VALIDATED                         
*                                                                               
         MVC   AUTSPV(L'AINFBYGR),AINFBYGR                                      
         MVC   AUTSPV+3(L'AINFUSCD),AINFUSCD                                    
         OI    AUTSPVH+6,X'80'     TRANSMIT                                     
*                                                                               
         GOTO1 HEXOUT,DMCB,AINFFLAG,AUTFLG,1                                    
         OI    AUTFLGH+6,X'80'     TRANSMIT                                     
*                                                                               
         LA    R2,AUTFLTH                                                       
         CLI   AINFMG,C'0'                                                      
         BE    DAUTH100                                                         
         LA    R2,AUTHIAH                                                       
         CLI   AINFMG,C'1'                                                      
         BE    DAUTH100                                                         
         LA    R2,AUTDHTH                                                       
         CLI   AINFMG,C'2'                                                      
         BE    DAUTH100                                                         
         LA    R2,AUTOFLH                                                       
         CLI   AINFMG,C'3'                                                      
         BE    DAUTH100                                                         
         LA    R2,AUTCMSH                                                       
         CLI   AINFMG,C'4'         TAKE CREDITS FOR ALL MISSED SPOTS            
         BE    DAUTH100                                                         
DAUTHERR DC    H'00'                                                            
DAUTH100 MVI   8(R2),C'Y'                                                       
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         EDIT  AINFMGWK,(L'AUTWMG,AUTWMG),ALIGN=LEFT                            
         OI    AUTWMGH+6,X'80'     DISPLAY WEEKS MAKEGOODS CAN RUN              
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         DROP  R6                                                               
*                                                                               
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
       ++INCLUDE SCSFMC2D          MAINTENACE SCREEN                            
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM91D          LIST SCREEN                                  
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENAUTH         SUPERDESK AUTHORIZATION REC DSECT            
         EJECT                                                                  
       ++INCLUDE SPGENEST          FOR ESTIMATE DATES                           
         EJECT                                                                  
       ++INCLUDE SPGENCLT          FOR PRODUCT MEMONIC                          
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
* START OF SAVED STORAGE (6144)                                                 
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    A                   RELOCATION FACTOR                            
LISTSEL  DS    C                   FOR USE WITH LIST SELECTIONS                 
AUTHKEY  DS    CL32                                                             
LSTKEY   DS    CL32                                                             
DSKADDR  DS    XL4                 DISK ADDRESS                                 
TEMPFLDH DS    XL11                TEMP FIELD FOR BLANK MEDIA                   
DATETEMP DS    CL6                 TEMP DATE FOR DATVAL                         
ELEMFLAG DS    XL1                 ELEM FLAG (USED TO CK EMPTY ELEM)            
LOOPCTR  DS    XL1                 LOOP COUNTER (FOR LOOPING)                   
*                                                                               
KEYAGYMD DS    XL1                                                              
KEYCLT   DS    XL2                                                              
KEYPRD   DS    XL1                                                              
KEYEST   DS    XL1                                                              
KEYMKT   DS    XL2                                                              
KEYSTA   DS    XL3                                                              
*                                                                               
ERRNUM   DS    XL2                                                              
SAVESEL  DS    CL1                                                              
SVREV    DS    XL1                                                              
SVVERSN  DS    XL1                                                              
MYLSTACT DS    X                   LAST ACTION                                  
CLTB     DS    XL2                 CLIENT CODE - BINARY                         
PRD      DS    CL3                 PRODUCT CODE                                 
PRDB     DS    X                   BINARY PRODUCT                               
SVDUEDT  DS    XL3                 SAVED DUE DATE                               
SVFLSDT  DS    XL3                 SAVED FLIGHT START DATE                      
FLAG1    DS    X                                                                
FL1NOSTA EQU   X'80'               DON'T DISPLAY STATIONS                       
FL1NOMKT EQU   X'40'               DON'T DISPLAY MARKETS                        
SVCKSM   DS    F                                                                
       ++INCLUDE FAGETTXTD         ERROR MSGS                                   
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSMED    DS    CL1                                                              
         DS    CL6                                                              
LSCLT    DS    CL3                                                              
         DS    CL5                                                              
LSPRD    DS    CL3                                                              
LSDASH   DS    CL1                                                              
LSPRD2   DS    CL3                                                              
         DS    CL2                                                              
LSEST    DS    CL3                                                              
LSDASH2  DS    CL1                                                              
LSVER    DS    CL1                                                              
         DS    CL5                                                              
LSMKT    DS    CL4                                                              
         DS    CL4                                                              
LSSTA    DS    CL8                                                              
         DS    CL1                                                              
LSREV    DS    CL3                                                              
         DS    CL2                                                              
LSDUE    DS    CL8                                                              
         DS    CL2                                                              
LSDSKADD DS    CL8                 OPTIONAL, DISPLAYS REC DISK ADDRESS          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009SPSFM02   04/26/16'                                      
         END                                                                    
