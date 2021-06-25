*          DATA SET NEMED22    AT LEVEL 005 AS OF 04/19/06                      
*          DATA SET NEMED22    AT LEVEL 187 AS OF 06/14/96                      
*PHASE T31E22A,+0                                                               
*INCLUDE PERVERT                                                                
         TITLE 'T31E22 - WEEKLY FLOWCHART'                                      
*******                                                                         
* INCLUDES DFSOPT - FOR DANCER REPORTING                                        
* IF DFSOPT SET TO Y THEN  1. UNDERLINE DATES IN HEADER                         
*                          2. SKIP ALINE BETWEEN NETWORKS                       
*                          3. IF GRP IS 0 PRINT A DASH (-)                      
*                                                                               
T31E22   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NTWE**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         LA    RA,1(RB)                                                         
         LA    RA,4095(RA)       RA = 2ND BASE REGISTER                         
         USING T31E22,RB,RA                                                     
*        L     RA,ATWA                                                          
*        USING T31EFFD,RA                                                       
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R7,ANETWS3                                                       
         USING NDDEMBLK,R7                                                      
         ST    R2,RELO                                                          
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         CLI   SPTLEN,C'Y'                                                      
         BNE   *+8                                                              
         MVI   RCSUBPRG,1                                                       
*                                                                               
         MVI   DFSOPT,C'Y'         FEATURE RELEASED TO ALL                      
         EJECT                                                                  
*                                                                               
*   INITIALIZE NETBLOCK                                                         
         MVI   NBNOWRIT,C'N'       INHIBIT WRITE                                
         OI    NBINDS,X'80'        EQUIVALENCE OVERRIDES                        
         OI    NBSPLOPT,X'80'      SPLIT OPTION BUT NOT ON POL                  
         CLI   MYFLAV,C'A'        USE ACTUALS?                                  
         BNE   IN6                                                              
         MVI   NBACTOPT,C'Y'                                                    
         CLI   MYFLAV+1,C'E'      FOR AE FLAVOR                                 
         BE    IN5                                                              
         MVI   NBSELUOP,C'A'       A FLAVOR                                     
         B     IN8                                                              
*                                                                               
IN5      MVI   NBESTOPT,C'A'       ESTIMATED DEMOS                              
         MVI   NBSELUOP,C'A'       ACTUAL SCEDULE                               
         B     IN8                                                              
*                                                                               
IN6      MVI   NBESTOPT,C'Y'       IF NOT USE ESTIMATES                         
         MVI   NBSELUOP,C'E'                                                    
*                                                                               
IN8      MVI   NBDATA,C'U'         GET UNITS                                    
*                                                                               
PROCDAT  NETGO NSNETIO,DMCB,NETBLOCK   PROCESS DATE                             
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBVALDAT                                                  
         BE    GOTDATE                                                          
         B     PROCDAT                                                          
*                                                                               
GOTDATE  DS    0H                                                               
         CLC   =C'MC',NBSELAGY                                                  
         BE    DOCLI                                                            
         CLC   =C'SJ',NBSELAGY                                                  
         BE    *+8                                                              
DOCLI    BAS   RE,GETCLINT                                                      
         BAS   RE,INITMON          INITIALIZE MONTH (WEEK) LIST                 
         LA    R1,TOTGOAL                                                       
         ST    R1,ATOTALS                                                       
         CLI   MYGOAL,C'Y'                                                      
         BNE   *+8                                                              
         BAS   RE,GETGOALS                                                      
         XC    NBACTNET,NBACTNET   *** FUDGE. ACTNET MAY HAVE BEEN SET          
*                                  *** WHEN READING PACKAGE, THUS WONT          
*                                  *** DETECT FIRST NET W/OUT FUDGE             
*                                                                               
GETUNIT  NETGO NSNETIO,DMCB,NETBLOCK    NOW DO UNIT RECORDS                     
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBREQLST     LAST ONE                                     
         BE    LASTONE                                                          
         CLI   NBMODE,NBPROCUN     IF A UNIT                                    
         BNE   GETUNIT                                                          
*                                                                               
CKPROG   CLI   MYSPOT,C'Y'        PRINT EACH UNIT FOR SPOT OPTION Y             
         BE    DOPROG                                                           
         CLC   OLDPROG,NBACTPRG                                                 
         BE    *+14                                                             
         MVC   OLDPROG,NBACTPRG                                                 
         B     DOPROG                                                           
         CLC   OLDNET,NBACTNET                                                  
         BE    GOTUNIT                                                          
DOPROG   BAS   RE,NEWPROG                                                       
         CLC   OLDNET,NBACTNET                                                  
         BE    GOTUNIT                                                          
         MVC   OLDNET,NBACTNET                                                  
         CLI   DFSOPT,C'Y'         SKIP A LINE 1ST FOR DFS OPT                  
         BNE   CKN2                                                             
         MVC   P(132),SPACES       BLANK LINE                                   
         BAS   RE,PRINTIT                                                       
         EDIT  (1,NBACTEST),(3,P+5)   REBUILD HEADER FOR NEW PROGRAM            
         MVC   P+13(6),NBACTPRG                                                 
         MVC   P+20(16),NBPROGNM                                                
CKN2     MVC   P(4),NBACTNET          MOVE IT TO PRINTLINE                      
GOTUNIT  BAS   RE,POST                                                          
         B     GETUNIT                                                          
*                                                                               
LASTONE  BAS   RE,NEWPROG          PRINT LAST PROGRAM                           
         CLI   SPTLEN,C'Y'                                                      
         BNE   *+8                                                              
         BAS   RE,SPTLENT                                                       
         CLI   MYNOPT,C'Y'        IF NET OPT=Y PRINT NET SUMMARY                
         BNE   LO2                                                              
         BAS   RE,NETSUMM                                                       
LO2      BAS   RE,REPTOTS          PRINT REPORT TOTALS                          
         B     XIT                                                              
*                                                                               
PROCERR  DC    H'0'                                                             
**************************************************************                  
         EJECT                                                                  
****************************************************************                
*  BUILD THE MONTH LIST. SET VALUES BASED ON CONTROLS.                          
*  IF WEEKS SPECIFIED, BUT SPAN IS GREATER THAN MONLIST ALLOWS, USE             
*      MONTHS INSTEAD.                                                          
*                                                                               
*  OUTPUTS:  MONLIST - LIST OF BEGIN-END DATE SETS FOR PERIOD.                  
*            NUMMONS - NUMBER OF USABLE DATE SETS IN LIST                       
*   LOCALS:                                                                     
*        R3 - A(FIRST DATE SET IN LIST)                                         
*        R4 - CURRENT NUMBER OF USABLE DATE SETS IN LIST                        
*                                                                               
INITMON  NTR1                                                                   
         CLI   DATOPT,C'D'         IS IT DAYS OPTION                            
         BE    INITDAYS                                                         
         MVC   PERTYPE(1),DATOPT   WEEK OR MONTH                                
         MVI   PERTYPE+1,1         SET SO MONS USED IF TOO MANY WKS             
         MVI   PERTYPE+2,0         DONT USE QUARTERS                            
         LA    R4,MAXMONTS                                                      
         ST    R4,NUMMONS          MAX NUMBER OF MONTHS                         
         NETGO NVWKLST,DMCB,NUMMONS,MONLIST,PERTYPE                             
IMXIT    B     XIT                                                              
*                                                                               
*                                                                               
INITDAYS DS    0H                  SET UP DAYS IN MONLIST                       
         GOTO1 =V(PERVERT),DMCB,NBSELSTR,NBSELEND,RR=RELO                       
         MVC   HALF,8(R1)                                                       
         LH    R2,HALF                                                          
         ST    R2,NUMMONS                                                       
         C     R2,=F'14'           A MAXIMUM OF 14 DAYS                         
         BNH   IND2                                                             
         LA    R2,14                                                            
         ST    R2,NUMMONS                                                       
         STH   R2,HALF                                                          
IND2     LA    R3,MONLIST                                                       
         MVC   WORKDAT,NBSELSTR                                                 
DATLOOP  GOTO1 DATCON,DMCB,(0,WORKDAT),(2,0(R3))                                
         CH    R2,HALF                                                          
         BNE   *+10                                                             
         MVC   NBCMPSTR,0(R3)                                                   
         MVC   2(2,R3),0(R3)                                                    
         MVC   NBSELEND,WORKDAT                                                 
         LA    R3,4(R3)                                                         
         GOTO1 ADDAY,DMCB,WORKDAT,WORKDAT,1    BUMP BY ONE DAY                  
         BCT   R2,DATLOOP                                                       
         A     R3,=F'-4'                                                        
         MVC   NBCMPEND,0(R3)                                                   
         B     IMXIT                                                            
         EJECT                                                                  
**************************************************                              
* GET GOALS                                                                     
*                                                                               
GETGOALS NTR1                                                                   
         GOTO1 NBCALLOV,DMCB,0,X'D9000A35'                                      
         L     RF,DMCB             PICK UP ADDRESS OF NETGOAL                   
         ST    RF,ANETGOAL                                                      
         L     R5,ANETWS1                GOAL RECS                              
         USING NETGOALD,R5                                                      
         XC    0(150,R5),0(R5)                                                  
         LA    R2,NETBLOCK                                                      
         ST    R2,NGANTBLK                                                      
         L     R2,ANETWS1                                                       
         LA    R2,150(R2)                                                       
         ST    R2,NGAPLIST                                                      
         MVI   NGMAXPRD,100        LIST=100 PRODS MAX                           
         MVI   NGEXTOPT,2          FILL LISTDEMO WITH 1ST 2 DEMOS               
         LA    R2,NETGOLHK                                                      
         ST    R2,NGAHOOK                                                       
         MVC   NGSELSL,NBSELLEN                                                 
         OI    NBINDS5,NBI5NOGL    DON'T REQAD GOAL HISTORY RECS                
         GOTO1 ANETGOAL,DMCB,NGBLOCK                                            
         B     XIT                                                              
NETGOLHK NTR1                                                                   
***      GOTO1 =V(PRNTBL),DMCB,=C'GOA',NGBLOCK,C'DUMP',100,=C'1D'               
         L     RE,NGOALGRP            SAVE GRP VALUE IN RE                      
         CLC   NGOALTRG,NDDEMOS+2     MATCH GOAL TARGET TO NDDEMOS              
         BE    NGH1                                                             
         CLC   NGOALTR2,NDDEMOS+2     IS IT SECOND DEMO ?                       
         BNE   NGHX                                                             
         L     RE,NGOALG2                                                       
NGH1     LA    R2,MONLIST                                                       
         L     R3,ATOTALS                                                       
         L     R4,NUMMONS                                                       
NGH3     CLC   NGOALWK,2(R2)                                                    
         BNH   NGH5                                                             
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,NGH3                                                          
         B     NGHX                                                             
NGH5     L     R1,0(R3)                                                         
****     A     R1,NGOALGRP                                                      
         AR    R1,RE                                                            
         ST    R1,0(R3)                                                         
NGHX     B     XIT                                                              
         EJECT                                                                  
************************************                                            
*  POST - PROCESS A UNIT RECORD                                                 
*                                                                               
POST     NTR1                                                                   
         MVC   DATE,NBACTDAT       SAVE OFF DATE                                
*                                                                               
         CLI   MYFLAV,C'E'        IF ESTIMATE DEMOS                             
         BE    USEESTS                                                          
         CLC   MYFLAV(2),=C'AE'   IF ACT SKED EST DEMOS                         
         BE    USEAESTS                                                         
         B     USEACTS                                                          
*                                                                               
USEAESTS MVC   UNITS,NBACTUN       FOR FLAVOR AE                                
         MVC   GRPS,NDESTDEM+2                                                  
         B     DU2                                                              
*                                                                               
USEESTS  MVC   UNITS,NBESTUN       FOR FLAVOR E                                 
         MVC   GRPS,NDESTDEM+2                                                  
         B     DU2                                                              
*                                                                               
USEACTS MVC   UNITS,NBACTUN        FOR FLAVOR A                                 
        MVC   GRPS,NDACTDEM+2                                                   
*                                                                               
*                          **  POST INTO ACCUMULATORS **                        
DU2      DS    0H                                                               
         L     R1,TOTUN                                                         
         AH    R1,UNITS                                                         
         ST    R1,TOTUN                                                         
*                                                                               
         LA    R6,BUEST               GET ESTIMATE                              
         LA    R1,15                      MAX OF 15 EST                         
         CLI   SPTLEN,C'Y'                                                      
         BNE   DU3A                                                             
         LA    R1,7                FOR SPTLEN MAX=7                             
DU3      CLI   SPTLEN,C'Y'                                                      
         BE    DU5                                                              
DU3A     CLC   3(1,R6),NBACTEST    IF MATCH OR EMPTY                            
         BE    DUEST                                                            
         OC    3(1,R6),3(R6)                                                    
         BZ    DUEST                                                            
         B     DU7                                                              
DU5      CLC   3(1,R6),NBLEN      IF MATCH OR EMPTY                             
         BE    DULEN                                                            
         OC    3(1,R6),3(R6)                                                    
         BZ    DULEN                                                            
DU7      LA    R6,72(R6)                                                        
         BCT   R1,DU3                                                           
         MVI   3(R6),0          16TH USE OTHERS                                 
         B     DU8                                                              
*                                                                               
DUEST    MVC   3(1,R6),NBACTEST                                                 
         B     DU8                                                              
DULEN    MVC   3(1,R6),NBLEN                                                    
*                                                                               
DU8      L     R1,4(R6)                                                         
         AH    R1,UNITS                                                         
         ST    R1,4(R6)                                                         
         LA    R2,MONLIST                                                       
         LA    R3,8(R6)                                                         
         LA    R4,TOTGRPS                                                       
         L     R5,NUMMONS                                                       
*                                                                               
         LA    R6,NETBUFF          GET APPROPRIATE NET BUFFER IN R6             
         LA    R1,14               MAX 14 NETWORKS                              
*                                  15TH IS OTHERS                               
NB17     CLC   0(4,R6),NBACTNET    IF MATCH OR EMPTY                            
         BE    NB18                                                             
         OC    0(4,R6),0(R6)                                                    
         BZ    NB18                                                             
         LA    R6,136(R6)                                                       
         BCT   R1,NB17                                                          
         MVC   0(4,R6),=C'OTH '    15TH USE OTHERS                              
         B     WE14                                                             
NB18     MVC   0(4,R6),NBACTNET                                                 
*                                                                               
WE14     L     R1,4(R6)            GET NET TOTAL UNITS                          
         AH    R1,UNITS            AND ADD CURRENT                              
         ST    R1,4(R6)                                                         
         LA    R6,8(R6)            POINT R6 TO GRPS                             
*                                                                               
WE15     CLC   DATE,2(R2)          LOCATE WEEK (MONTH)                          
         BNH   WE16                                                             
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         LA    R6,4(R6)                                                         
         BCT   R5,WE15                                                          
***      DC    H'0'                MNTH (WEEK) NOT FOUND                        
         B     POSTX               EXIT/DON'T DUMP-THERE ARE UNITS              
*                                  OUTSIDE EST DATES                            
*                                                                               
*                                                                               
WE16     L     R1,0(R3)            ADD GRPS TO THESE SLOTS                      
         AH    R1,GRPS                                                          
         ST    R1,0(R3)                                                         
         L     R1,0(R4)                                                         
         AH    R1,GRPS                                                          
         ST    R1,0(R4)                                                         
         L     R1,64(R4)                                                        
         LA    R1,1(R1)                                                         
         ST    R1,64(R4)           UNIT REPORT TOTS FOR WEEK/MNTH               
         L     R1,0(R6)                                                         
         AH    R1,GRPS                                                          
         ST    R1,0(R6)                                                         
         L     R1,64(R6)                                                        
         LA    R1,1(R1)                                                         
         ST    R1,64(R6)           UNIT NET TOTS FOR WEEK/MNTH                  
POSTX    B     XIT                                                              
*                                                                               
*********************************                                               
         EJECT                                                                  
*******************************                                                 
**  NEWPROG - BEGINS PROCESSING FOR A NEW PROGRAM                               
*                                                                               
NEWPROG  NTR1                                                                   
         LA    R2,BUEST                                                         
         USING BUEST,R2                                                         
         MVI   BUEND,X'FF'                                                      
         DROP  R2                                                               
         LA    R2,BUEST            DUMP THE OLD PROGRAM                         
         OC    BUYUN(68),BUYUN                                                  
         BZ    DOHEAD              (UNLESS THIS IS THE FIRST PROG)              
         CLI   SPTLEN,C'Y'         IF REPORTING SPOT LEN                        
         BNE   WE24                             THEN SORT                       
         LA    R1,7                                                             
         SR    R3,R3                                                            
WE20     OC    0(4,R2),0(R2)                                                    
         BZ    WE21                                                             
         LA    R3,1(R3)                                                         
         LA    R2,72(R2)                                                        
         BCT   R1,WE20                                                          
WE21     LA    R2,BUEST                                                         
         GOTO1 XSORT,DMCB,(0,(R2)),(R3),72,4,0                                  
         LA    R2,BUEST            NOW ADD TOSPOT LEN TOTALS                    
         LA    R6,576(R2)                                                       
         OC    0(4,R6),0(R6)                                                    
         BNZ   FINDROW                                                          
         LR    RF,R6                                                            
         LR    RE,R2                                                            
         L     R1,=F'576'                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
         B     WE21A                                                            
FINDROW  DS    0H                                                               
         LR    R3,R2               SAVE R2=BUEST                                
         LR    R4,R6               SAVE R6=SPOT TOTALS                          
FND      LA    R1,7                                                             
FND1     CLC   0(4,R2),0(R6)                                                    
         BE    ADDIT                                                            
         OC    0(4,R6),0(R6)                                                    
         BZ    ADDIT                                                            
         LA    R6,72(R6)                                                        
         BCT   R1,FND1                                                          
         DS    H'0'                                                             
ADDIT    MVC   0(4,R6),0(R2)                                                    
         L     R1,4(R2)                                                         
         A     R1,4(R6)                                                         
         ST    R1,4(R6)                                                         
         L     R5,NUMMONS                                                       
ADT2     L     R1,8(R2)                                                         
         A     R1,8(R6)                                                         
         ST    R1,8(R6)                                                         
         LA    R2,4(R2)                                                         
         LA    R6,4(R6)                                                         
         BCT   R5,ADT2                                                          
         LA    R3,72(R3)           ARE THERE MORE SPOT LENGTHS                  
         OC    0(4,R3),0(R3)                                                    
         BZ    WE21A                                                            
         LA    R2,BUEST                                                         
         LA    R2,576(R2)                                                       
         CR    R3,R2                                                            
         BNL   WE21A                                                            
         LR    R6,R2                                                            
         LR    R2,R3                                                            
         B     FND                                                              
*                                                                               
WE21A    DS    0H                                                               
         LA    R2,BUEST                                                         
         LA    R1,7                                                             
WE22     BAS   RE,FORMAT                                                        
         XC    0(72,R2),0(R2)                                                   
         LA    R2,72(R2)                                                        
         OC    0(4,R2),0(R2)                                                    
         BZ    DOHEAD                                                           
         BCT   R1,WE22                                                          
         B     DOHEAD                                                           
*                                                                               
WE24     BAS   RE,FORMAT                                                        
         XC    0(72,R2),0(R2)                                                   
         LA    R2,72(R2)                                                        
         OC    0(4,R2),0(R2)                                                    
         BZ    DOHEAD                                                           
         CLI   0(R2),X'FF'                                                      
         BNE   WE24                                                             
*                                                                               
DOHEAD   EDIT  (1,NBACTEST),(3,P+5)    BUILD HEADER FOR NEW PROGRAM             
         MVC   P+13(6),NBACTPRG                                                 
         MVC   P+20(16),NBPROGNM                                                
         B     XIT                                                              
         EJECT                                                                  
*********************************                                               
* REPTOTS - PRINT THE REPORT TOTALS                                             
*                                                                               
REPTOTS  NTR1                                                                   
         MVI   UNTOPT,1                                                         
         MVI   RPTOTS,C'Y'                                                      
         LA    R2,TOTUN                                                         
         MVC   P,SPACES                                                         
         BAS   RE,PRINTIT                                                       
         MVC   P+26(6),=C'TOTALS'                                               
         CLI   MYUNIT,C'Y'                                                      
         BNE   REPT5                                                            
         BAS   RE,FORMAT                                                        
         B     REPTX                                                            
REPT5    BAS   RE,FORMATA                                                       
REPTX    B     XIT                                                              
********************                                                            
         SPACE 2                                                                
*********************************                                               
* NETSUMM - PRINT THE NETWORK SUMMARY TOTALS                                    
*                                                                               
NETSUMM  NTR1                                                                   
         MVI   UNTOPT,1                                                         
         LA    R3,15               MAX NETWORKS                                 
         LA    R6,NETBUFF                                                       
NS2      MVC   P,SPACES                                                         
         BAS   RE,PRINTIT                                                       
         OC    0(4,R6),0(R6)       CK FOR NO MORE NETWORKS                      
         BZ    XITNS                                                            
         MVC   P+20(4),0(R6)                                                    
         MVC   P+26(6),=C'TOTALS'                                               
         LA    R2,4(R6)            R2 IS ARGUMENT TO FORMAT.                    
         BAS   RE,FORMATA                                                       
         LA    R6,136(R6)                                                       
         BCT   R3,NS2                                                           
XITNS    MVC   P,SPACES                                                         
         BAS   RE,PRINTIT                                                       
         MVI   UNTOPT,0                                                         
         B     XIT                                                              
********************                                                            
         EJECT                                                                  
*******************************************                                     
*              FORMAT A LINE OF ACCUMULATORS                                    
*                                                                               
FORMAT   NTR1                                                                   
         CLI   MYUNIT,C'Y'           IS UNIT TOTS OPT ON                        
         BNE   FM1                                                              
         CLI   UNTOPT,1               CHK IF NETSUM/REPTOTS                     
         BNE   FM1                                                              
         MVC   P3+26(5),=C'UNITS'     * FOR UNIT OPTION                         
         B     FM1A                                                             
FORMATA  NTR1                                                                   
FM1A     EDIT  (4,0(R2)),(4,P3+36)                                              
         LA    R2,4(R2)                                                         
         B     FM1B                                                             
FM1      EDIT  (4,4(R2)),(4,P+36)                                               
         EDIT  (B4,0(R2)),(3,P+5)                                               
         CLI   3(R2),0             IS IT'OTHER' ESTIMATE                        
         BNE   *+10                                                             
         MVC   P+5(3),=C'XXX'                                                   
         LA    R2,8(R2)                                                         
FM1B     LA    R3,P+41                                                          
         L     R4,NUMMONS                                                       
         SR    R5,R5                                                            
*                                                                               
*                                                                               
FM2      OC    0(4,R2),0(R2)                                                    
         BNZ   FM3                                                              
         CLI   DFSOPT,C'Y'         FOR DFS OPT, PRINT - IF NO GRP               
         BNE   FM3                 PXZ CHANGE(WAS FM4)                          
         MVI   2(R3),C'-'                                                       
         B     FM4                                                              
*                                                                               
FM3      A     R5,0(R2)                      ADD UP GRPS IN R5                  
         EDIT  (4,0(R2)),(4,0(R3)),1                                            
         TM    NBINDS3,NBI3A2DC    2 DEC AGY?                                   
         BNO   FM3AA                                                            
         EDIT  (4,0(R2)),(5,0(R3)),2                                            
*                                                                               
FM3AA    CLI   MYUNIT,C'Y'        IS UNIT TOTS OPT ON                           
         BNE   FM3A                                                             
         CLI   UNTOPT,1            CHK IF NETSUM/REPTOTS                        
         BNE   FM3A                                                             
         MVI   P2,0                 *                                           
         LA    R6,264(R3)           *                                           
         EDIT  (4,64(R2)),(4,0(R6)) *  UNITS                                    
*                                                                               
FM3A     CLC   0(4,R2),=F'1000'              ALWAYS HAVE ROOM FOR 99.9          
         BL    FM4                                                              
         MVC   0(4,R3),SPACES                                                   
         TM    NBINDS3,NBI3A2DC                                                 
         BNO   *+10                                                             
         MVC   0(5,R3),SPACES                                                   
         LR    R6,R3                                                            
         SH    R6,=H'4'                                                         
         CLC   0(8,R6),SPACES                BIG NUMBERS MAY FIT                
         BE    *+8                                                              
         LA    R6,132(R6)                    OR GO ON THE LINE BELOW            
         EDIT  (4,0(R2)),(8,0(R6)),1                                            
         TM     NBINDS3,NBI3A2DC                                                
         BNO   FM4                                                              
         EDIT  (4,0(R2)),(8,0(R6)),2                                            
*                                                                               
FM4      LA    R2,4(R2)                                                         
         LA    R3,5(R3)                                                         
         TM    NBINDS3,NBI3A2DC                                                 
         BNO   *+8                                                              
         LA    R3,1(R3)            BUMP FOR EXTRA 2 DEC AGY CHARACTER           
         BCT   R4,FM2                                                           
         CLI   GRPOPT,C'Y'                  DO WE NEED TO SHOW GRPTOT           
         BNE   FM6                                                              
         EDIT  (R5),(6,P+13),1                                                  
         TM    NBINDS3,NBI3A2DC                                                 
         BNO   FM5                                                              
         EDIT  (R5),(7,P+13),2                                                  
FM5      C     R5,=F'100000'                                                    
         BL    FM6                                                              
         EDIT  (R5),(7,P+13)                                                    
         MVI   P+19,C' '                                                        
*                                                                               
FM6      BAS   RE,PRINTIT                                                       
         CLI   RPTOTS,C'Y'         IF ITS REPORT TOTAL TIME                     
         BNE   XIT                    THEN GO ON /ELSE XIT                      
*                                                                               
*                                **** FORMAT GOALS ****                         
         CLI   MYGOAL,C'Y'        GOAL OPTION                                   
         BNE   XIT                                                              
*                                                                               
         BAS   RE,PRINTIT                                                       
*                                                                               
         CLI   GRPOPT,C'Y'            DO WE NEED TO SHOW GOALTOT                
         BNE   FM6A                                                             
*                                                                               
         LA    R3,15                                                            
         XC    FULL,FULL                                                        
         LA    R2,TOTGOAL          ADD ALL GOALS                                
GLOOP    L     R1,0(R2)                                                         
         A     R1,FULL                                                          
         ST    R1,FULL                                                          
         LA    R2,4(R2)                                                         
         BCT   R3,GLOOP                                                         
         EDIT  (B4,FULL),(6,P+13),1                                             
         CLC   FULL,=F'100000'                                                  
         BL    FM6A                                                             
         EDIT  (B4,FULL),(7,P+13)                                               
         MVI   P+19,C' '                                                        
*                                                                               
FM6A     MVC   P+26(5),=C'GOALS'                                                
         LA    R3,P+41                                                          
         L     R4,NUMMONS                                                       
         LA    R2,TOTGOAL                                                       
FM8      OC    0(4,R2),0(R2)                                                    
         BNZ   FM10                                                             
         MVI   2(R3),C'-'                                                       
         B     FM12                                                             
*                                                                               
FM10     DS    0H                                                               
         EDIT  (4,0(R2)),(4,0(R3)),1                                            
         CLC   0(4,R2),=F'1000'              ALWAYS HAVE ROOM FOR 99.9          
         BL    FM12                                                             
         MVC   0(4,R3),SPACES                                                   
         LR    R6,R3                                                            
         SH    R6,=H'4'                                                         
         CLC   0(8,R6),SPACES                BIG NUMBERS MAY FIT                
         BE    *+8                                                              
         LA    R6,132(R6)                    OR GO ON THE LINE BELOW            
         EDIT  (4,0(R2)),(8,0(R6)),1                                            
FM12     LA    R2,4(R2)                                                         
         LA    R3,5(R3)                                                         
         BCT   R4,FM8                                                           
*                                                                               
         BAS   RE,PRINTIT                                                       
XIT      XIT1                                                                   
********************************                                                
         EJECT                                                                  
***********************************                                             
* FORMAT SPOT LENGTH TOTALS                                                     
*                                                                               
SPTLENT  NTR1                                                                   
         XC    P,P                                                              
         MVI   SPACING,2                                                        
         BAS   RE,PRINTIT                                                       
         SR    R2,R2                                                            
         LA    R4,7                                                             
         LA    R3,BUEST                                                         
         LA    R3,576(R3)                     POINT TO SPTLEN TOTS              
SPL00    OC    0(4,R3),0(R3)                                                    
         BZ    SPL0                                                             
         LA    R2,1(R2)                                                         
         LA    R3,72(R3)                                                        
         BCT   R4,SPL00                                                         
SPL0     LA    R3,BUEST                                                         
         LA    R3,576(R3)                     POINT TO SPTLEN TOTS              
         GOTO1 XSORT,DMCB,(0,(R3)),(R2),72,4,0                                  
         LA    R2,P                                                             
         LR    R6,R3                          SAVE START OF TOTS                
SPL1     EDIT (B4,4(R3)),(4,168(R2))                                            
         MVC   20(9,R2),=C'SPOT LEN:'                                           
         EDIT  (B4,0(R3)),(3,29(R2)),ALIGN=LEFT                                 
         LA    R2,P+41                                                          
         L     R4,NUMMONS                                                       
SPL2     OC    8(4,R3),8(R3)                                                    
         BNZ   SPL2A                                                            
         MVI   2(R2),C'-'                                                       
         B     SPL4                                                             
SPL2A    EDIT  (B4,8(R3)),(4,0(R2)),1                                           
         L     R1,LENTOT                                                        
         A     R1,8(R3)                                                         
         ST    R1,LENTOT                                                        
         CLC   8(4,R3),=F'1000'              ALWAYS HAVE ROOM FOR 99.9          
         BL    SPL4                                                             
         MVC   0(4,R2),SPACES                                                   
         LR    R5,R2                                                            
         SH    R5,=H'4'                                                         
         CLC   0(8,R5),SPACES                BIG NUMBERS MAY FIT                
         BE    *+8                                                              
         LA    R5,132(R5)                    OR GO ON THE LINE BELOW            
         EDIT  (4,8(R3)),(8,0(R5)),1                                            
SPL4     LA    R2,5(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,SPL2                                                          
SPL6     DS    0H                                                               
         LA    R2,P                                                             
         EDIT  (B4,LENTOT),(7,12(R2)),1                                         
         XC    LENTOT,LENTOT                                                    
         MVI   SPACING,2                                                        
         BAS   RE,PRINTIT                                                       
         CLI   MYGOAL,C'Y'                                                      
         BNE   SPL8                                                             
         MVC   NBSELLEN,3(R6)      GET GOAL TOTS                                
         LA    R1,TOTGOAL2                                                      
         ST    R1,ATOTALS                                                       
         BAS   RE,GETGOALS                                                      
         BAS   RE,WRITEGLS                                                      
         XC    TOTGOAL2,TOTGOAL2                                                
SPL8     LA    R6,72(R6)                                                        
         OC    0(4,R6),0(R6)                                                    
         BZ    SPLX                                                             
         CLI   0(R6),X'FF'                                                      
         BE    SPLX                                                             
         BAS   RE,PRINTIT          SKIP A LINE                                  
         LR    R3,R6                                                            
         LA    R2,P                                                             
         B     SPL1                                                             
SPLX     B     XIT                                                              
         EJECT                                                                  
*****************************************                                       
*                                                                               
* GET GOAL SUBTOTS AND PRINT                                                    
*                                                                               
WRITEGLS NTR1                                                                   
         MVC   P+24(5),=C'GOALS'                                                
         LA    R3,P+41                                                          
         L     R4,NUMMONS                                                       
         LA    R2,TOTGOAL2                                                      
WR8      OC    0(4,R2),0(R2)                                                    
         BNZ   WR10                                                             
         MVI   2(R3),C'-'                                                       
         B     WR12                                                             
*                                                                               
WR10     DS    0H                                                               
         EDIT  (4,0(R2)),(4,0(R3)),1                                            
         CLC   0(4,R2),=F'1000'              ALWAYS HAVE ROOM FOR 99.9          
         BL    WR12                                                             
         MVC   0(4,R3),SPACES                                                   
         LR    R6,R3                                                            
         SH    R6,=H'4'                                                         
         CLC   0(8,R6),SPACES                BIG NUMBERS MAY FIT                
         BE    *+8                                                              
         LA    R6,132(R6)                    OR GO ON THE LINE BELOW            
         EDIT  (4,0(R2)),(8,0(R6)),1                                            
WR12     LA    R2,4(R2)                                                         
         LA    R3,5(R3)                                                         
         BCT   R4,WR8                                                           
         MVI   SPACING,2                                                        
         BAS   RE,PRINTIT                                                       
         B     XIT                                                              
*                                                                               
         SPACE 2                                                                
*                                                                               
PRINTIT  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
***********************************                                             
*              HEADLINE ROUTINES                                                
*                                                                               
HOOK     NTR1                                                                   
         L     R5,ATWA                                                          
         USING T31EFFD,R5                                                       
         MVC   H4+10(3),SPLCLI                                                  
         MVC   H5+10(7),SPLPRO                                                  
         MVC   H6+10(3),SPLEST                                                  
         MVC   H4+14(20),SPLCLIN                                                
         MVC   H5+18(20),SPLPRON                                                
         MVC   H6+14(24),SPLESTN                                                
         CLI   NBSELESE,0                                                       
         BE    HOOK1                                                            
         MVC   H6+10(7),SPLEST                                                  
         MVI   H6+17,C' '                                                       
         MVC   H6+18(24),SPLESTN                                                
         OC    H6+10(32),SPACES                                                 
         GOTO1 SQUASHER,DMCB,H6+10,32                                           
*                                                                               
HOOK1    LA    R2,H1+47            TITLE                                        
         MVC   H1+40(6),=C'WEEKLY'                                              
         MVC   H2+40(8),=8C'-'                                                  
         CLI   PERTYPE,C'W'                                                     
         BE    HOOK2                                                            
         MVC   H1+40(7),=C'MONTHLY'                                             
         LA    R2,1(R2)                                                         
         CLI   DATOPT,C'D'                                                      
         BNE   HOOK2                                                            
         MVC   H1+40(7),=C' DAILY '                                             
*                                                                               
HOOK2    MVC   0(17,R2),=C'RATINGS FLOWCHART'                                   
         MVC   132(17,R2),=17C'-'                                               
         MVC   H5+82(3),=C'ALL'                                                 
         OC    NBSELDP,NBSELDP                                                  
         BZ    HOOK3                                                            
         MVC   H5+82(8),SPLDPTN                                                 
*                                                                               
*                                  GET NAME OF 1ST DEMO IN WORK                 
HOOK3    DS    0H                                                               
*        PRINT GEN                                                              
*        NETGO NVDEMCON,DMCB,(0,NDDEMBLK),DBLOCK,(7,WORK)                       
*        PRINT NOGEN                                                            
         MVC   H6+82(7),DEMNMSV                                                 
*                                                                               
         CLI   SPLFLAV,C'E'                                                     
         BNE   HOOK3A                                                           
         MVC   H6+90(16),=C'EST SCHD/EST DEM'                                   
         B     HOOK4                                                            
HOOK3A   CLC   SPLFLAV(2),=C'AE'                                                
         BNE   HOOK3B                                                           
         MVC   H6+90(16),=C'ACT SCHD/EST DEM'                                   
         B     HOOK4                                                            
HOOK3B   CLI   SPLFLAV,C'A'                                                     
         BNE   HOOK4                                                            
         MVC   H6+90(16),=C'ACT SCHD/ACT DEM'                                   
*                                                                               
HOOK4    LA    R2,MONLIST                                                       
         LA    R3,H10+42                                                        
         L     R4,NUMMONS                                                       
*                                                                               
HOOK6    CLI   PERTYPE,C'M'                                                     
         BE    HOOK8                                                            
         GOTO1 DATCON,DMCB,(2,0(R2)),(4,WORK)                                   
         MVC   0(3,R3),WORK                                                     
         MVC   133(2,R3),WORK+3                                                 
         CLI   DFSOPT,C'Y'                                                      
         BNE   HOOK10                                                           
         MVC   264(3,R3),=C'---'                                                
         B     HOOK10                                                           
*                                                                               
HOOK8    GOTO1 DATCON,DMCB,(2,2(R2)),(6,WORK)                                   
         MVC   0(3,R3),WORK                                                     
         MVC   133(2,R3),WORK+4                                                 
         CLI   DFSOPT,C'Y'                                                      
         BNE   HOOK10                                                           
         MVC   264(3,R3),=C'---'                                                
*                                                                               
HOOK10   LA    R2,4(R2)                                                         
         LA    R3,5(R3)                                                         
         TM    NBINDS3,NBI3A2DC                                                 
         BNO   *+8                                                              
         LA    R3,1(R3)                                                         
         BCT   R4,HOOK6                                                         
         CLI   GRPOPT,C'Y'                                                      
         BNE   XITHOOK                                                          
         MVC   H10+13(6),=C' TOTAL'                                             
         MVC   H11+13(6),=C' GRPS.'                                             
         CLI   DFSOPT,C'Y'         FOR DFS OPT                                  
         BNE   XITHOOK                                                          
         MVC   H12+13(6),=C'------'  CHANGE UNDERLINING                         
         MVC   H11(3),SPACES                                                    
         MVC   H12(3),=C'---'                                                   
         MVC   H11+5(3),SPACES                                                  
         MVC   H12+5(3),=C'---'                                                 
         MVC   H11+20(12),SPACES                                                
         MVC   H12+20(12),=C'-------------'                                     
         MVC   H11+35(5),SPACES                                                 
         MVC   H12+35(5),=C'-----'                                              
*                                                                               
XITHOOK  B     XIT                                                              
         DROP  R5                                                               
***************************************8                                        
         EJECT                                                                  
*                                                                               
GETCLINT NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),NBACTAM  AGY/MED/CLIENT                                 
         MVC   KEYSAVE(13),KEY                                                  
         GOTO1 DATAMGR,DMCB,=CL8'DMRDHI',=C'SPTDIR',KEY,KEY,0                   
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,CLISAVE                                                       
         ST    R2,NBACLI                                                        
         GOTO1 NBDM,DMCB,=CL8'GETREC',=C'SPTFILE ',KEY+14,(R2),DMWORK           
         XIT1                                                                   
*              LTORG                                                            
         SPACE 3                                                                
         LTORG                                                                  
*                                                                               
CLISAVE  DS    CL2000                                                           
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE NENETGOALD                                                     
         PRINT ON                                                               
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDF2D                                                       
         EJECT                                                                  
*                                                                               
         DSECT                                                                  
       ++INCLUDE NETDEMOT                                                       
       ++INCLUDE DEDBLOCK                                                       
*                                                                               
         EJECT                                                                  
* ARGS FROM EDIT                                                                
DPFILT   DS    CL1                                                              
FLAVOR   DS    CL1                                                              
DATOPT   DS    CL1                                                              
GRPOPT   DS    CL1                                                              
DFSOPT   DS    CL1                 SPECIAL DFS FEATURES                         
DEMNMSV  DS    CL7                                                              
         DS    CL5                 DO NOT USE DEMNMSV NEEDS IT                  
SPTLEN   DS    CL1                                                              
MYFLAV   DS    CL2                                                              
MYSPOT   DS    CL1                                                              
MYUNIT   DS    CL1                                                              
MYGOAL   DS    CL1                                                              
MYNOPT   DS    CL1                                                              
*                                                                               
MAXMONTS EQU   16                  MAXIMUM NUMBER OF MONTHS (WEEKS)             
NUMMONS  DS    F                   NUMBER OF MONTHS IN LIST                     
MONLIST  DS    CL(4*MAXMONTS)      MONTH (WEEK) LIST                            
PERTYPE  DS    CL3                 PERIOD TYPE AND CONTROL BYTES                
*                                                                               
NETBUFF  DS    2040C               SPACE FOR 15 LINES OF                        
*                                  CL4 - NETWORK                                
*                                  CL4 - UNITS                                  
*                                  CL64 - GRPS                                  
*                                  CL64 - UNITS PER WEEK/MONTH                  
TOTUN    DS    F                                                                
TOTGRPS  DS    CL64                                                             
TOTUNITS DS    CL64                UNIT TOTS PER WEEK/MONTH                     
DATE     DS    CL2                                                              
UNITS    DS    H                                                                
GRPS     DS    H                                                                
UNTOPT   DS    CL1                                                              
         DS    0F                                                               
LENTOT   DS    F                                                                
ANETGOAL DS    F                                                                
ATOTALS  DS    F                                                                
TOTGOAL  DS    CL64                                                             
TOTGOAL2 DS    CL64                                                             
RPTOTS   DS    CL1                                                              
WORKDAT  DS    CL6                                                              
RELO     DS    F                                                                
OLDNET   DS    CL4                                                              
OLDPROG  DS    CL6                                                              
*                                                                               
BUEST    DS    F                                                                
BUYUN    DS    CL4                                                              
BUYGRPS  DS    CL64                                                             
         DS    CL360               SPACE FOR 5 MORE ESTIMATES                   
         DS    CL720               + SPACE FOR 10 MORE ESTIMATES                
         DS    CL72                + 1 MORE                                     
BUEND    DS    CL1                                                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005NEMED22   04/19/06'                                      
         END                                                                    
