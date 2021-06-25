*          DATA SET NENETDRIVE AT LEVEL 023 AS OF 05/01/02                      
*PHASE T00A41A                                                                  
         TITLE 'NETDRIV - DRIVER FOR NETWORK SPOOL'                             
NETDRIV  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 NDWORKX-NDWORK,**NEDV**,RR=R2                                    
         USING NDWORK,RC                                                        
         L     RA,0(R1)                                                         
         USING GLOBALD,RA                                                       
         L     R8,GLAWORKD                                                      
         USING GEND,R8                                                          
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
*                                                                               
*                                                                               
******************************                                                  
         CLI   GLHOOK,GLRESOLV                                                  
         BNE   ND2                                                              
         BAS   RE,SYSINIT                                                       
         B     XIT                                                              
*                                                                               
ND2      CLI   GLHOOK,GLROUT                                                    
         BNE   XIT                                                              
         CLI   GLMODE,GLINPUT                                                   
         BNE   ND6                                                              
         BAS   RE,SYSINP                                                        
         B     XIT                                                              
ND6      CLI   GLMODE,GLOUTPUT                                                  
         BNE   ND10                                                             
         BAS   RE,SYSOUTP                                                       
         B     XIT                                                              
ND10     DC    H'0'                                                             
XIT      XIT1                                                                   
         SPACE 1                                                                
         EJECT                                                                  
********************************************                                    
* SYSTEM INITIALLIZATION                                                        
********************************************                                    
*                                                                               
SYSINIT  NTR1                                                                   
         LA    R1,ROUTLIST                                                      
         LA    R2,GLLABEL                                                       
         SPACE 1                                                                
SYSINIT2 CLC   0(8,R1),0(R2)                                                    
         BE    SYSINIT4                                                         
         LA    R1,12(R1)                                                        
         CLI   0(R1),X'FF'                                                      
         BE    XIT                                                              
         B     SYSINIT2                                                         
         SPACE 1                                                                
SYSINIT4 MVC   GLAROUT,8(R1)                                                    
         B     XIT                                                              
         SPACE 1                                                                
ROUTLIST DS    0F                                                               
         DC    C'NINET   ',A(NINET)                                             
         DC    C'NIPCODE ',A(NIPROGCD)                                          
         DC    C'NIPROGNM',A(NIPROGNM)                                          
         DC    C'NILEN   ',A(NILEN)                                             
*                                                                               
         DC    C'NIASSIGN',A(NIASSIGN)                                          
         DC    C'NIIMP   ',A(NIIMP)                                             
         DC    C'NIGRP   ',A(NIGRP)                                             
         DC    C'NHIMP   ',A(NHIMP)                                             
         DC    C'NHGRP   ',A(NHGRP)                                             
         DC    C'DEMOUT  ',A(NHDEM)                                             
         DC    X'FF'                                                            
         EJECT                                                                  
*******************************************                                     
* SYSOUTP                                                                       
*******************************************                                     
SYSOUTP  NTR1                                                                   
         L     RF,GLAROUT                                                       
         L     R2,GLAIFLD                                                       
         L     R3,GLAOFLD                                                       
         BR    RF                                                               
         EJECT                                                                  
*****************************************                                       
*  INPUT ROUTINES                                                               
***************************************                                         
SYSINP   NTR1                                                                   
         L     RF,GLAROUT                                                       
         L     R3,GLAIFLD                                                       
         BR    RF                                                               
*                                                                               
*        EJECT                                                                  
******************************************************************              
* SIMPLE ROUTINES                                                               
*  INPUT R3 - AREA TO MOVE DATA TO                                              
*  PERFORM A STRAIGHT DATA MOVE                                                 
*                                                                               
NIAGY    MVC   0(L'NBALPHA,R3),NBALPHA         AGENCY ALPHA                     
         B     XIT                                                              
*                                                                               
NICLICOD MVC   0(L'NBCLICOD,R3),NBCLICOD          CLIENT ALPHA                  
         B     XIT                                                              
*                                                                               
NIPRFILT MVC   0(L'NBPRFILT,R3),NBPRFILT          PROGRAM FILTER                
         B     XIT                                                              
*                                                                               
NIDAYNAM MVC   0(L'NBDAYNAM,R3),NBDAYNAM          DAY  (3 CHAR)                 
         B     XIT                                                              
*                                                                               
NIFEEDMG MVC   0(L'NBFEEDMG,R3),NBFEEDMG          FEED MARKET GROUP             
         B     XIT                                                              
*                                                                               
NIHUTAVE MVC   0(L'NBHUTAVE,R3),NBHUTAVE          HUT AVERAGING USED            
         B     XIT                                                              
*                                                                               
NIMGFPCD MVC   0(L'NBMGFPCD,R3),NBMGFPCD          M/G FOR PROG CODE             
         B     XIT                                                              
*                                                                               
NIMGBPCD MVC   0(L'NBMGBPCD,R3),NBMGBPCD          M/G BY PROG CODE              
         B     XIT                                                              
*                                                                               
NIMGFPNM MVC   0(L'NBMGFPNM,R3),NBMGFPNM          M/G FOR PROG NAME             
         B     XIT                                                              
*                                                                               
NIMGBPNM MVC   0(L'NBMGBPNM,R3),NBMGBPNM          M/G BY PROG NAME              
         B     XIT                                                              
*                                                                               
NIRESULT MVC   0(L'NBRESULT,R3),NBRESULT         RESULT CODE FOR DEMOS          
         B     XIT                                                              
*                                                                               
NIDAYP   MVC   0(L'NBACTDP,R3),NBACTDP             DAYPART (1-CHAR)             
         B     XIT                                                              
*                                                                               
NIEST    MVC   0(L'NBACTEST,R3),NBACTEST           ESTIMATE NUM                 
         B     XIT                                                              
*                                                                               
NIPRDN   MVC   0(L'NBPRD,R3),NBPRD                 PRODUCT NUM                  
         B     XIT                                                              
*                                                                               
NIPRDN2  MVC   0(L'NBPRD2,R3),NBPRD2               SECOND PROD NUMBER           
         B     XIT                                                              
*                                                                               
NIPACK   MVC   0(L'NBPACK,R3),NBPACK               PACKAGE NUMBER               
         B     XIT                                                              
*                                                                               
NINTI    MVC   0(L'NBNTI,R3),NBNTI                 NTI NUMBER                   
         B     XIT                                                              
*                                                                               
NINSI    MVC   0(L'NBNSI,R3),NBNSI                 NSI NUMBER                   
         B     XIT                                                              
*                                                                               
NIUNCODE MVC   0(L'NBUNCODE,R3),NBUNCODE           UNIVERSE CODE                
         B     XIT                                                              
*                                                                               
NIMARKET MVC   0(L'NBMARKET,R3),NBMARKET            MARKET NUM                  
         B     XIT                                                              
*                                                                               
NISREP   MVC   0(L'NBSREP,R3),NBSREP                SPECIAL REP                 
         B     XIT                                                              
*                                                                               
NIHUTPCT MVC   0(L'NBHUTPCT,R3),NBHUTPCT            HUT %                       
         B     XIT                                                              
*                                                                               
NIFEED   MVC   0(L'NBFEED,R3),NBFEED                FEED %                      
         B     XIT                                                              
*                                                                               
NIUNIV   MVC   0(L'NBUNIV,R3),NBUNIV                UNIV %                      
         B     XIT                                                              
*                                                                               
NIIMPACT MVC   0(L'NBIMPACT,R3),NBIMPACT            IMPACT %                    
         B     XIT                                                              
*                                                                               
NIUNITST MVC   0(L'NBUNITST,R3),NBUNITST            UNIT STATUS                 
         B     XIT                                                              
*                                                                               
NIPACKST MVC   0(L'NBPACKST,R3),NBPACKST            PACKAGE STATUS              
         B     XIT                                                              
*                                                                               
NIACTWHY MVC   0(L'NBACTWHY,R3),NBACTWHY         REASON 4 LAST ACTIVITY         
         B     XIT                                                              
*                                                                               
NIDAY    MVC   0(L'NBDAY,R3),NBDAY                  DAY MASK                    
         B     XIT                                                              
*                                                                               
NIDATE   MVC   0(L'NBACTDAT,R3),NBACTDAT            DATE                        
         B     XIT                                                              
*                                                                               
NIMGFDAT MVC   0(L'NBMGFDAT,R3),NBMGFDAT            M/G FOR DATE                
         B     XIT                                                              
*                                                                               
NIMGBDAT MVC   0(L'NBMGBDAT,R3),NBMGBDAT            M/G BY DATE                 
         B     XIT                                                              
*                                                                               
NITIME   MVC   0(L'NBTIME,R3),NBTIME                START-END TIME              
         B     XIT                                                              
*                                                                               
NIAFFTIM MVC   0(L'NBAFFTIM,R3),NBAFFTIM            AFFIDAVIT TIME              
         B     XIT                                                              
*                                                                               
NINET    MVC   0(L'NBACTNET,R3),NBACTNET         NETWORK                        
         B     XIT                                                              
*                                                                               
NIPROGNM MVC   0(L'NBPROGNM,R3),NBPROGNM         PROGRAM NAME                   
         B     XIT                                                              
*                                                                               
NIPROGCD MVC   0(L'NBACTPRG,R3),NBACTPRG         PROGRAM CODE                   
         B     XIT                                                              
*                                                                               
NIACTUAL MVC   0(L'NBACTUAL,R3),NBACTUAL         ACTUAL COST                    
         B     XIT                                                              
*                                                                               
NIASSIGN MVC   0(L'NBASSIGN,R3),NBASSIGN         ASSIGNED COST                  
         B     XIT                                                              
*                                                                               
NIINTEG  MVC   0(L'NBINTEG,R3),NBINTEG           INTEGRATION COST               
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
***************************************************************                 
* INPUT ROUTINES REQUIRING MINOR EXPANSION  (NO ARGS)                           
*                                                                               
NILEN    ZIC   R1,NBLEN            LENGTH                                       
         ST    R1,0(R3)            1-BYTE SAVED AS FULLWORD                     
         B     XIT                                                              
******************************************************************              
*                                                                               
*                                                                               
******************************************************************              
*     ALPHA SUB LINE                                                            
*     4 -BYTE MAX OUTPUT IS PRECEDED BY A -                                     
*                                                                               
NISUB    MVC   0(4,R3),=C'  '      DEFAULT TO SPACES                            
         CLI   NBACTSUB,1          IF SUBLINE NE 1                              
         BE    NISBXIT                                                          
         ZIC   R2,NBACTSUB          THEN PUT IT IN PRINTABLE FORMAT             
         EDIT  (R2),(3,1(R3)),ALIGN=LEFT                                        
NISBXIT  B     XIT                                                              
******************************************************************              
*                                                                               
*                                                                               
******************************************************************              
*     M/G FOR ALPHA SUB LINE                                                    
*     4 -BYTE MAX OUTPUT IS PRECEDED BY A -                                     
*                                                                               
NIMGFSUB MVC   0(4,R3),=C'  '      DEFAULT TO SPACES                            
         CLI   NBMGFSUB,1          IF SUBLINE NE 1                              
         BE    NIFSBXIT                                                         
         ZIC   R2,NBMGFSUB          THEN PUT IT IN PRINTABLE FORMAT             
         EDIT  (R2),(3,1(R3)),ALIGN=LEFT                                        
NIFSBXIT B     XIT                                                              
******************************************************************              
*                                                                               
*                                                                               
******************************************************************              
*     M/G BY ALPHA SUB LINE                                                     
*     4 -BYTE MAX OUTPUT IS PRECEDED BY A -                                     
*                                                                               
NIMGBSUB MVC   0(4,R3),=C'  '      DEFAULT TO SPACES                            
         CLI   NBMGBSUB,1          IF SUBLINE NE 1                              
         BE    NIBSBXIT                                                         
         ZIC   R2,NBMGBSUB          THEN PUT IT IN PRINTABLE FORMAT             
         EDIT  (R2),(3,1(R3)),ALIGN=LEFT                                        
NIBSBXIT B     XIT                                                              
**************************                                                      
*                                                                               
*                                                                               
**************************                                                      
*BILLED DATE  (USE WHICHEVER IS FILLED IN)                                      
*   NOTE: WE ARE ASSURED THAT TIME AND INTEG DATES ARE EQUAL                    
*                                                                               
NIBILDT  L     R6,AACCBLK                                                       
         USING NACCBLKD,R6                                                      
         LTR   R6,R6                                                            
         BNZ   BDT1                CK IF AN ACCTG BLOCK SET-UP                  
         DC    H'0'                                                             
BDT1     OC    NACBTDAT,NACBTDAT                                                
         BZ    BDT2                                                             
         MVC   0(2,R3),NACBTDAT    TIME DATE                                    
         B     XITBDT                                                           
BDT2     MVC   0(2,R3),NACBIDAT    INTEG DATE                                   
XITBDT   B     XIT                                                              
         DROP  R6                                                               
**************************                                                      
*                                                                               
*                                                                               
**************************                                                      
*PAID DATE  (USE WHICHEVER IS FILLED IN)                                        
*   NOTE: WE ARE ASSURED THAT TIME AND INTEG DATES ARE EQUAL                    
*                                                                               
NIPAYDT  L     R6,AACCBLK                                                       
         USING NACCBLKD,R6                                                      
         LTR   R6,R6                                                            
         BNZ   PDT1                                                             
PDT1     OC    NACBTDAT,NACPTDAT                                                
         BZ    PDT2                                                             
         MVC   0(2,R3),NACPTDAT    TIME DATE                                    
         B     XITPDT                                                           
PDT2     MVC   0(2,R3),NACPIDAT    INTEG DATE                                   
XITPDT   B     XIT                                                              
         DROP  R6                                                               
*******************************************************                         
*                                                                               
*                                                                               
****************************************************                            
* NICALTIM - CALCULATED TIME. USE AFFIDAVIT TIME IF THERE, ELSE USE             
*     START-END TIME                                                            
*                                                                               
NICALTIM XC    0(4,R3),0(R3)       INITIALIZE 4 BYTES TO 0                      
         OC    NBAFFTIM,NBAFFTIM   IF AFFID TIME                                
         BZ    CALTM2                                                           
         MVC   0(2,R3),NBAFFTIM      USE FIRST 2 BYTES, LEAVE REST 0            
         B     CALTMXIT                                                         
CALTM2   MVC   0(4,R3),NBTIME      ELSE USE 4-BYTE START-END TIME               
*                                                                               
CALTMXIT B     XIT                                                              
*******************************************************                         
*                                                                               
*                                                                               
*******************************************************                         
* NIPRDCD - RETURN 3-BYTE PRODUCT CODE                                          
*   REQUIRES A CLIENT HEADER AS INPUT                                           
*                                                                               
*                                                                               
NIPRDCD  L     R6,NBACLI           A(CLIENT RECORD) FROM NETBLOCK               
         USING CLTHDR,R6                                                        
         LTR   R6,R6                                                            
         BNZ   PCD2                                                             
         DC    H'0'                BOMB IF NO CLI REC                           
PCD2     LA    R2,CLIST                                                         
         LA    R5,220                                                           
         CLI   NBPRD,0             IF PROD CODE IS ZERO, RETURN 'UNA'           
         BNE   PCD4                                                             
         MVC   0(3,R3),=C'UNA'                                                  
         B     PCDXIT                                                           
PCD4     CLI   0(R2),X'FF'                                                      
         BE    PCDXIT                                                           
PCDLOP   CLC   NBPRD(1),0(R2)                                                   
         BE    PCDGOT                                                           
         LA    R2,4(R2)                                                         
         BCT   R5,PCDLOP                                                        
PCDGOT   MVC   0(3,R3),0(R2)                                                    
PCDXIT   B     XIT                                                              
***********************************************                                 
NIPRDCD2 DC    H'0'                NOT DONE YET                                 
NIBILPCD DC    H'0'                NOT DONE YET                                 
         EJECT                                                                  
***********************************************                                 
**   COMPLEX INPUT ROUTINES - REQUIRE ARGUMENTS                                 
***********************************************                                 
*                                                                               
*                                                                               
***********************************************                                 
* NIIMP - IMPRESSION                                                            
*  ARGUMENTS: BYTE 2 - A OR E (DEFAULT LOOKS TO NBESTOPT,NBACTOPT)              
*             BYTE 1 - RELATIVE DEMO NUMBER                                     
*                                                                               
NIIMP    BAS   RE,DEMADJ           POINTS R4 TO PROPER DEMO GROUP               
         MVC   0(4,R3),4(R4)       MOVE IMPRESSION                              
         B     XIT                                                              
***************************************************                             
*                                                                               
*                                                                               
***********************************************                                 
* NIGRP - RATING                                                                
*  ARGUMENTS: BYTE 2 - A OR E (DEFAULT LOOKS TO NBESTOPT,NBACTOPT)              
*             BYTE 1 - RELATIVE DEMO NUMBER                                     
*      RETURNS RATING AS A 4 BYTE FIELD                                         
*                                                                               
NIGRP    BAS   RE,DEMADJ           POINTS R4 TO PROPER DEMO GROUP               
         SR    R2,R2                                                            
         ICM   R2,3,2(R4)          TAKE 2 BYTES                                 
         ST    R2,0(R3)            AND STORE AS 4                               
         B     XIT                                                              
***************************************************                             
*                                                                               
*                                                                               
***********************************************                                 
* NIVPH - VPH                                                                   
*  ARGUMENTS: BYTE 2 - A OR E (DEFAULT LOOKS TO NBESTOPT,NBACTOPT)              
*             BYTE 1 - RELATIVE DEMO NUMBER                                     
*      RETURNS VPH AS A 4 BYTE FIELD                                            
*                                                                               
NIVPH    BAS   RE,DEMADJ           POINTS R4 TO PROPER DEMO GROUP               
         SR    R2,R2                                                            
         ICM   R2,3,0(R4)          TAKE 2 BYTES                                 
         ST    R2,0(R3)            AND STORE AS 4                               
         B     XIT                                                              
***************************************************                             
         EJECT                                                                  
***************************************                                         
* DEMADJ - RETURNS R4 POINTING TO THE CORRECT 8 BYTE DEMO                       
*          INPUTS: GLARGS                                                       
*                           BYTE1 - RELATIVE DEMO NUMBER (0=HOMES)              
*                           BYTE2 - E OR A                                      
*                                    IF 0: USE E IF NBESTOPT SET,               
*                                              A IF NBACTOPT SET                
*                                              DUMP IF BOTH OR NONE SET         
*          OUTPUT: R4 - A(8 BYTE DEMO BLOCK)                                    
*          LOCALS: BYTE - SET TO E OR A                                         
*                                                                               
DEMADJ   NTR1                                                                   
         MVC   BYTE,GLARGS+1       MOVE ARG2 TO BYTE                            
         CLI   BYTE,C'E'           IF NOT E OR A                                
         BE    DMAJGETD                                                         
         CLI   BYTE,C'A'                                                        
         BE    DMAJGETD                                                         
         CLI   NBESTOPT,0            IF ESTOPT SET                              
         BE    DMAJ4                                                            
         CLI   NBACTOPT,0              IF ACTOPT SET                            
         BE    DMAJ2                                                            
         DC    H'0'                        BOMB                                 
*                                      ELSE                                     
DMAJ2    MVI   BYTE,C'E'                 USE EST DEMOS                          
         B     DMAJGETD                                                         
*                                    ELSEIF ACTOPT SET                          
DMAJ4    CLI   NBACTOPT,0                                                       
         BE    DMAJ6                                                            
         MVI   BYTE,C'A'                USE ACT DEMOS                           
         B     DMAJGETD                                                         
*                                    ELSE  (NEITHER SET)                        
DMAJ6    DC    H'0'                    BOMB. NO DEMOS REQUESTED                 
*                                                                               
DMAJGETD CLI   GLARGS,0         IF HOMES                                        
         BNE   OTHERTG                                                          
         SR    R2,R2                                                            
         LA    R4,NBESTHOM                                                      
         CLI   BYTE,C'A'                                                        
         BNE   DADJXIT                                                          
         LA    R4,NBACTHOM                                                      
         B     DADJXIT                                                          
*                                                                               
OTHERTG  L     R5,NBADEM                                                        
         LTR   R5,R5               DEMO BLOCK MUST EXIST                        
         BNZ   DADJB                                                            
         DC    H'0'                                                             
         USING NDDEMBLK,R5                                                      
DADJB    LA    R4,NDESTDEM                                                      
         CLI   BYTE,C'A'         ESTIMATED DEMOS                                
         BNE   DADGC                                                            
         LA    R4,NDACTDEM                                                      
DADGC    ZIC   R2,GLARGS         DEMO NUMBER                                    
         BCTR  R2,0                CALCULATE OFFSET                             
         SLL   R2,3                                                             
         LA    R4,0(R4,R2)         POINTS TO PROPER DEMO                        
DADJXIT  XIT1  REGS=(R4)                                                        
         DROP  R5                                                               
*****************************************************                           
NHIMP    B     XIT                                                              
NHGRP    B     XIT                                                              
         EJECT                                                                  
*******************************************                                     
* NHDEM - RETUNS 7-BYTE DEMO FOR THE HEADER                                     
*         INPUTS- GLARGS (R6)- BYTE 1-RELATIVE DEMO NUMBER (0= HOMES)           
*                          BYTE 2 - TYPE (0 MEANS TAKE FROM DEMLIST)            
*        LOCALS : R5 - NET DEMO BLOCK                                           
*                 WORK - OUTPUT FROM DEMOCON HERE, THEN MOVED TO OUTPUT         
*                                                                               
*******************************************                                     
*                 DBLOCK - NEEDED FOR DEMOCON                                   
*                                                                               
NHDEM    L     R5,NBADEM                                                        
         LTR   R5,R5                                                            
         BNZ   NHD2                                                             
         DC    H'0'                                                             
         USING NDDEMBLK,R5                                                      
*                                                                               
NHD2     ZIC   R6,GLARGS           DEMO NUMBER                                  
         LTR   R6,R6               IF R6=0 THEN HOMES                           
         BNZ   NHD5                                                             
         MVI   FAKEDEMO,0          SET UP A FAKE HOMES REQUEST                  
         MVC   FAKEDEMO+1(1),GLARGS+1                                           
         MVI   FAKEDEMO+3,0                                                     
         B     NHD6                                                             
NHD5     BCTR  R6,0                CALCULATE OFFSET                             
         MH    R6,=H'3'              EACH DEMO IS 3-BYTES                       
         LA    R6,NDDEMOS(R6)      R6 PTS TO PROPER DEMO                        
         MVC   FAKEDEMO(3),0(R6)                                                
         CLI   GLARGS+1,0           IF NO ARG THEN USE WHATS IN NDDEMOS         
         BE    NHD6                                                             
         MVC   FAKEDEMO+1(1),GLARGS+1   ELSE SET IT TO THE ARG                  
NHD6     CLI   FAKEDEMO+1,X'21'    IF A USER DEMO                               
         BE    VVDCUSER                                                         
         CLI   FAKEDEMO+1,63       IF A WEIGHTED DEMO                           
         BE    VVDCWGHT                                                         
         B     VVDOCON                                                          
*                                                                               
VVDCWGHT MVC   0(7,R3),NDWGTNAM    USE WEIGHTED NAME                            
         B     VVHDEND                                                          
*                                                                               
VVDCUSER ZIC   R1,FAKEDEMO+2       GET USER DEMO NUMBER                         
         BCTR  R1,0                DECREMENT TO CALC OFFSET                     
         MH    R1,=H'7'            USER NAMES ARE 7 BYTES                       
         LA    R1,NDUSRNMS(R1)     ADRESS OF USER DEMO                          
         MVC   0(7,R3),0(R1)                                                    
         B     VVHDEND                                                          
*                                                                               
VVDOCON  XC    DBLOCK,DBLOCK       SET UP DBLOCK                                
         MVC   DBFILE,=C'NTI'                                                   
         MVC   DBCOMFCS,NBACOM                                                  
         MVI   DBSELMED,C'N'                                                    
         MVI   DBSELSRC,C'N'                                                    
         LA    R2,7                                                             
VVDOC2   GOTO1 NBCALLOV,DMCB,0,X'D9000AE0'     A(DEMOCON)                       
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(0,FAKEDEMO),((R2),WORK),(C'S',DBLOCK)                 
         MVC   0(7,R3),WORK                                                     
VVHDEND  B     XIT                                                              
         DROP  R5                                                               
************************                                                        
*******************************************                                     
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*****************************************                                       
*W/S FOR NETDRIV                                                                
NDWORK   DSECT                                                                  
FAKEDEMO DS    CL3                 HOLDS A 3-BYTE DEMO CODE                     
*                                                                               
*                                  NEED A DEMO BLOCK AROUND                     
       ++INCLUDE DEDBLOCK                                                       
*                                                                               
NDWORKX  EQU   *                   END OF W/S                                   
*                                                                               
         EJECT                                                                  
       ++INCLUDE DRGLOBAL                                                       
         EJECT                                                                  
*              NETINCLS AND MODULE W/S                                          
         SPACE 3                                                                
       ++INCLUDE NETINCLS                                                       
         SPACE 1                                                                
       ++INCLUDE NETDEMOD                                                       
*                                                                               
       ++INCLUDE SPGENCLT                                                       
*                                                                               
       ++INCLUDE NEACCTBLK                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023NENETDRIVE05/01/02'                                      
         END                                                                    
