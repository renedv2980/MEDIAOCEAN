*          DATA SET SPTRA51    AT LEVEL 007 AS OF 02/15/07                      
*PHASE T21651B                                                                  
         TITLE 'T21651 MARKET LIST DISPLAY, CHANGE, ADD LIST'                   
***********************************************************************         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)                   
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER                
*             AIO2 -                                                            
*             AIO3 -                                                            
*                                                                               
* REGISTER USAGE -                                                              
*        R0 - WORK REG                                                          
*        R1 - WORK REG                                                          
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR                 
*        R3 - WORK REG                                                          
*        R4 - WORK REG & KEY DSECT POINTER                                      
*        R5 - WORK REG - IN LIST RTN POINTER TO LIST/PRINT LINE                 
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
*                                                                     *         
* LEV 02 SMUR MAR29/01 USE TRAFFIC OFFICE                             *         
* LEV 03 SMUR JUL09/02 CLIENT STRING SECURITY                         *         
* LEV 06 SMUR MAY18/06 MODIFY VALIEST CALL TO WORK W/MORE BRANDS CONTR*         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
T21651   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21651**,RR=R3                                                 
         USING T21651,RB                                                        
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,SPTR51RR                                                      
         SPACE 3                                                                
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LR                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*     VALIDATE KEY ROUTINE                                                      
         SPACE                                                                  
VK       LA    R2,TRAMEDH          FIELD PTR FOR MEDIA                          
         GOTO1 VALIMED                                                          
         SPACE                                                                  
         LA    R2,TRACLTH          CLIENT                                       
         XC    BCLT,BCLT                                                        
         XC    QCLT,QCLT                                                        
         SPACE                                                                  
         CLI   5(R2),0                                                          
         BNE   VK05                                                             
         SPACE                                                                  
         TM    WHEN,X'30'          SOON/OV                                      
         BZ    VK03                 NO                                          
         SPACE                                                                  
* MUST ENTER CLIENT IF CLIENT STRING SECURITY                                   
         SPACE                                                                  
         MVI   ERROR,SECLOCK       PRESET SECURITY LOCKOUT                      
         SPACE                                                                  
         L     RF,ASECBLK                                                       
         USING SECD,RF                                                          
         SPACE                                                                  
         OC    SECCLAL,SECCLAL     CLIENT STRING?                               
         BNZ   TRAPERR                                                          
         SPACE                                                                  
         MVI   ERROR,0                                                          
         SPACE                                                                  
VK03     CLI   ACTNUM,ACTLIST      LIST                                         
         BE    VK10                                                             
         SPACE                                                                  
VK05     GOTO1 ANY                                                              
         GOTO1 VALICLT                                                          
         SPACE                                                                  
         BAS   RE,GETPRF           READ PROFILES                                
         SPACE                                                                  
VK10     LA    R2,TRAPRDH          PRODUCT                                      
         MVI   BPRD,0                                                           
         XC    QPRD,QPRD                                                        
         CLI   5(R2),0                                                          
         BNE   VK20                YES                                          
         CLI   ACTNUM,ACTLIST                                                   
         BE    VK30                                                             
         CLI   SVT1PR16,C'Y'       BY PROD?                                     
         BNE   VK30                NO                                           
         SPACE                                                                  
         GOTO1 ANY                                                              
         SPACE                                                                  
VK20     LA    R2,TRACLTH                                                       
         CLI   5(R2),0             WAS CLIENT ENTERED                           
         BNE   VK22                 YES                                         
         GOTO1 ANY                                                              
         SPACE                                                                  
VK22     LA    R2,TRAPRDH                                                       
         GOTO1 VALIPRD                                                          
         CLC   =C'POL',WORK                                                     
         BE    INVPRDER            PRD POL NOT ALLOWED                          
         MVC   QPRD,WORK                                                        
         MVC   BPRD,WORK+3                                                      
         SPACE                                                                  
VK30     LA    R2,TRAESTH                                                       
         SPACE                                                                  
         XC    QEST,QEST                                                        
         MVI   BEST,0                                                           
         SPACE                                                                  
         CLI   5(R2),0                                                          
         BNE   VK32                                                             
         CLI   SVT1PR11,C'E'       COPY CODE ESTIMATE                           
         BNE   VK35                                                             
         CLI   ACTNUM,ACTLIST                                                   
         BE    VK35                                                             
VK32     LA    R2,TRAPRDH          WAS PRD ENTERED?                             
         CLI   5(R2),0                                                          
         BNE   VK33                YES                                          
         GOTO1 ANY                                                              
VK33     LA    R2,TRAESTH                                                       
         GOTO1 VALIEST,DMCB,0                                                   
         SPACE                                                                  
VK35     LA    R2,TRAMLNH                                                       
         XC    SVMKTLN,SVMKTLN     INIT MARKET LIST NAME                        
         CLI   5(R2),0                                                          
         BE    *+14                                                             
         MVC   SVMKTLN,TRAMLN      SAVE MARKET LIST NAME                        
         B     VK40                                                             
         SPACE                                                                  
         CLI   ACTNUM,ACTLIST                                                   
         BE    VK40                                                             
         SPACE                                                                  
         GOTO1 ANY                                                              
         SPACE                                                                  
* NOW BUILD KEY                                                                 
         SPACE                                                                  
VK40     LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING MKLKEY,R4                                                        
         MVC   MKLKID,=XL2'0A38'                                                
         MVC   MKLKAM,BAGYMD                                                    
         MVC   MKLKCLT,BCLT        MOVE IN CLIENT                               
         MVC   MKLKBPRD,BPRD            PRODUCT CODE                            
         MVC   MKLKBEST,BEST            ESTIMATE CODE                           
         MVC   MKLKLNAM,TRAMLN          MARKET                                  
         B     EXIT                                                             
         DROP  R4                                                               
         SPACE                                                                  
TRAPERR  GOTO1 ERREX                                                            
         EJECT                                                                  
* VALIDATE RECORD ROUTINE                                                       
         SPACE 3                                                                
VR       LA    R3,2000             LENGTH FOR MOVE                              
         LR    RF,R3               FOR BOTH FIELDS                              
         L     RE,AIO2             TO                                           
         L     R2,AIO1             FROM                                         
         MVCL  RE,R2                                                            
         SPACE                                                                  
         L     R4,AIO2                                                          
         ST    R4,AIO                                                           
         MVC   SVKEY,KEY                                                        
         USING MKLKEY,R4                                                        
         SPACE                                                                  
         MVC   MKLAGYA,AGENCY                                                   
         SPACE                                                                  
         LA    R2,TRAMKT1H         FIRST FLD                                    
         MVI   ELCODE,X'10'        DATA ELEMENT                                 
         L     R6,AIO2                                                          
         GOTO1 REMELEM             REMOVE ALL MARKET CODES                      
         MVI   MKTSW,0                                                          
         SPACE                                                                  
VR10     LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING MKLDTAEL,R6                                                      
         MVI   ELCODE,X'10'                                                     
         MVI   MKLDTAEL,X'10'      ELEMENT IDENTIFIER                           
         MVI   MKLDTALN,MKLDTAX-MKLDTAEL ELEMENT LENGTH                         
         CLI   5(R2),0             ANY DATA ENTERED                             
         BE    VR30                NO                                           
         SPACE                                                                  
         GOTO1 VALIMKT                                                          
         SPACE                                                                  
*  MARKET CODE MUST BE RIGHT JUSTIFIED                                          
         SPACE                                                                  
         XC    FULL,FULL                                                        
         SPACE                                                                  
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2) *EXECUTED*                                           
         SPACE                                                                  
         EDIT  (P8,DUB),(4,FULL),FILL=0                                         
         SPACE                                                                  
* CHECK FOR DUPLICATE ENTRY                                                     
         SPACE                                                                  
VR12     LA    R5,TRAMKT1H         FIRST FLD                                    
         LA    R1,TRATAGH          END OF SCREEN                                
         SPACE                                                                  
VR13     CR    R5,R2                                                            
         BE    VR15                                                             
         CLC   8(L'TRAMKT1,R5),SPACES IF NO ENTRY                               
         BNH   VR15                    THEN SKIP                                
         CLC   FULL,8(R5)             CHK FOR DUPLICATE ENTRY                   
         BE    DUPERR                                                           
VR15     ZIC   R0,0(R5)            GET LEN                                      
         AR    R5,R0               GET TO NEXT FIELD                            
         CR    R5,R1                                                            
         BL    VR13                                                             
         SPACE                                                                  
VR20     MVC   MKLMKT,BMKT         MOVE IN MARKET CODE                          
         SPACE                                                                  
VR24     MVC   AIO,AIO2                                                         
         GOTO1 ADDELEM                                                          
         MVI   MKTSW,1                                                          
         SPACE                                                                  
VR30     ZIC   R0,0(R2)            GET LEN OF THIS FLD                          
         AR    R2,R0               POINT TO PTR CMML                            
         LA    R1,TRATAGH          SEE IF AT END OF SCREEN                      
         CR    R2,R1                                                            
         BL    VR10                                                             
         CLI   MKTSW,1             ANY MARKET CODES ENTERED                     
         BNE   NOMKTERR                                                         
         SPACE                                                                  
         MVC   KEY(L'SVKEY),SVKEY                                               
         MVC   AIO,AIO1            DO NOT DESTROY UPDATED                       
         CLI   ACTNUM,ACTADD                                                    
         BE    VR38                                                             
         SPACE                                                                  
         GOTO1 GETREC                                                           
         SPACE                                                                  
VR38     LA    R3,2000             LENGTH FOR MOVE                              
         LR    RF,R3               FOR BOTH FIELDS                              
         L     R2,AIO2             FROM                                         
         L     RE,AIO1             TO                                           
         MVCL  RE,R2                                                            
         B     DR                  GO DISPLAY UPDATED RECORD                    
         SPACE                                                                  
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY RECORD                                                                
         SPACE                                                                  
DR       LA    R2,TRAMKT1H                                                      
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING MKLDTAEL,R6                                                      
         SPACE                                                                  
DR10     XC    WORK(4),WORK                                                     
         SR    R0,R0                                                            
         ICM   R0,3,MKLMKT                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(4),DUB                                                      
         SPACE                                                                  
DR16     CLC   8(L'TRAMKT1,R2),WORK                                             
         BE    *+14                                                             
         MVC   8(L'TRAMKT1,R2),WORK                                             
         OI    6(R2),X'80'                                                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BAS   RE,NEXTEL                                                        
         BE    DR10                                                             
         BAS   RE,CLR                                                           
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY KEY                                                                   
         SPACE 3                                                                
DK       LA    R2,TRAMEDH                                                       
         L     R4,AIO                                                           
         USING MKLKEY,R4                                                        
         MVC   TRAMED,QMED         MOVE IN MEDIA                                
         OI    TRAMEDH+6,X'80'     SET ON TRANSMIT BIT                          
         SPACE                                                                  
         GOTO1 CLUNPK,DMCB,MKLKCLT,WORK                                         
         MVC   TRACLT,WORK         MOVE IN CLIENT                               
         OI    TRACLTH+6,X'80'     SET ON TRANSMIT BIT                          
         SPACE                                                                  
         CLI   MKLKBPRD,0          ANY PRODUCT?                                 
         BE    DK30                 NO, BYPASS                                  
         SPACE                                                                  
         OC    SVBCLT,SVBCLT       1ST TIME                                     
         BNZ   *+14                                                             
         MVC   SVBCLT,KEY+3        MOVE IN CLT                                  
         B     DK05                                                             
         CLC   SVBCLT,KEY+3                                                     
         BE    DK06                                                             
         SPACE                                                                  
         MVC   SVBCLT,KEY+3                                                     
         SPACE                                                                  
DK05     GOTO1 =A(FCLT),RR=SPTR51RR                                             
         BNE   EXIT                                                             
         SPACE                                                                  
DK06     LA    R0,220              MAX PRDS                                     
         L     R1,ASVCLIST         TABLE OF CLIENT PROD CODES                   
DK10     CLI   0(R1),C' '          AT END OF TABLE?                             
         BH    *+6                                                              
         DC    H'0'                PROBLEM!!!                                   
         SPACE                                                                  
         CLC   3(1,R1),MKLKBPRD    THIS A VALID PROD CODE                       
         BE    DK15                                                             
         LA    R1,4(,R1)           BUMP PROD PTR                                
         BCT   R0,DK10                                                          
         DC    H'0'                                                             
DK15     MVC   TRAPRD,0(R1)        MOVE IN PROD                                 
         OI    TRAPRDH+6,X'80'     SET ON TRANSMIT BIT                          
         SPACE                                                                  
DK20     CLI   MKLKBEST,0          ANY ESITMATE                                 
         BE    DK30                 NO                                          
         EDIT  (B1,MKLKBEST),(3,QEST),ALIGN=LEFT                                
         MVC   TRAEST,QEST         DISPLAY ESTIMATE                             
         OI    TRAESTH+6,X'80'     TRANSMIT                                     
         SPACE                                                                  
DK30     MVC   TRAMLN,MKLKLNAM     MOVE IN MARKET LIST NAME                     
         OI    TRAMLNH+6,X'80'     SET ON TRANSMIT BIT                          
         SPACE                                                                  
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* ONLINE LIST OR OFFLINE REPORT ROUTINE                                         
         SPACE                                                                  
LR       LA    R4,KEY                                                           
         MVI   NLISTS,16                                                        
         XC    SVBCLT,SVBCLT                                                    
         USING MKLKEY,R4                                                        
         OC    KEY(13),KEY         FIRST TIME ?                                 
         BNZ   LR20                NO, GOTO HIGH                                
         LA    R1,HEADING          HEADING LINE FOR REPORT                      
         ST    R1,SPECS            STORE FOR CONTROLLER                         
         LA    R1,HDHK             HEADING ROUTINE                              
         ST    R1,HEADHOOK         STORE FOR CONTROLLER                         
         SPACE                                                                  
* BUILD KEY, AND DO READHI                                                      
         SPACE                                                                  
         XC    KEY,KEY                                                          
         USING MKLKEY,R4                                                        
         MVC   MKLKID,=XL2'0A38'                                                
         MVC   MKLKAM,BAGYMD                                                    
         MVC   MKLKCLT,BCLT        MOVE IN CLIENT                               
         MVC   MKLKBPRD,BPRD            PRODUCT CODE                            
         MVC   MKLKBEST,BEST            ESTIMATE CODE                           
         MVC   MKLKLNAM,TRAMLN          MARKET                                  
         SPACE                                                                  
         GOTO1 HIGH                GO DO DATAMGR READ HI                        
         SPACE                                                                  
         CLC   KEYSAVE(3),KEY      WERE THERE ANY RECS FOR THIS AGENCY          
         BE    LR20                                                             
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BNE   EXIT                                                             
         MVC   P(26),=CL26'NO MARKET LIST RECS FOUND'                           
         GOTO1 SPOOL,DMCB,(R8)     NO RECORDS AT ALL                            
         B     EXIT                                                             
         SPACE                                                                  
LR10     GOTO1 SEQ                 DO READ SEQUENTIAL                           
         SPACE                                                                  
LR20     CLC   KEY(3),KEYSAVE      AT END OF THIS AGENCY/MED                    
         BNE   EXIT                 YES                                         
         OC    BCLT,BCLT           WAS CLIENT ENTERED                           
         BNZ   LR25                 YES                                         
         SPACE                                                                  
         OC    SVBCLT,SVBCLT       1ST TIME                                     
         BNZ   *+14                                                             
         MVC   SVBCLT,KEY+3        MOVE IN CLT                                  
         B     LR23                                                             
         CLC   SVBCLT,KEY+3                                                     
         BE    LR30                                                             
         SPACE                                                                  
         MVC   SVBCLT,KEY+3                                                     
         SPACE                                                                  
LR23     GOTO1 =A(FCLT),RR=SPTR51RR                                             
         BNE   EXIT                                                             
         GOTO1 CLUNPK,DMCB,SVBCLT,QCLT                                          
         B     LR30                                                             
         SPACE                                                                  
LR25     CLC   BCLT,KEY+3                                                       
         BNE   LR10                                                             
         SPACE                                                                  
LR30     CLI   BPRD,0                                                           
         BE    LR35                                                             
         CLC   BPRD,KEY+5                                                       
         BNE   LR10                                                             
         SPACE                                                                  
LR35     CLI   BEST,0              WAS ESTIMATE ENTERED                         
         BE    LR36                 NO                                          
         CLC   BEST,KEY+6                                                       
         BNE   LR10                                                             
         SPACE                                                                  
LR36     OC    SVMKTLN,SVMKTLN     MARKET LIST NAME                             
         BZ    LR40                                                             
         CLC   SVMKTLN,KEY+7                                                    
         BNE   LR10                                                             
         SPACE                                                                  
LR40     GOTO1 GETREC                                                           
         SPACE                                                                  
         L     R4,AIO                                                           
         TM    15(R4),X'80'        DELETED RECORD                               
         BO    LR10                BYPASS                                       
         LR    R6,R4                                                            
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                HAS TO BE AT LEAST 1 MARKET                  
         USING MKLDTAEL,R6                                                      
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LRL                 GO DO ONLINE LIST                            
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LRR                 GO FORMAT FOR OFFLINE REPORT                 
         DC    H'0'                MUST BE ON/OFFLINE                           
         EJECT                                                                  
* FORMAT ONLINE LIST HERE                                                       
         SPACE                                                                  
LRL      MVC   LISTAR,SPACES                                                    
         SPACE                                                                  
         MVC   LCLT,QCLT                                                        
         OC    MKLKBPRD,MKLKBPRD   ANY PROD                                     
         BZ    LRL20                                                            
         SPACE                                                                  
         LA    R0,220              MAX PRDS                                     
         L     R1,ASVCLIST         TABLE OF CLIENT PROD CODES                   
LRL10    CLI   0(R1),C' '          AT END OF TABLE?                             
         BH    *+6                                                              
         DC    H'0'                PROBLEM!!!                                   
         SPACE                                                                  
         CLC   3(1,R1),MKLKBPRD    THIS A VALID PROD CODE                       
         BE    LRL15                                                            
         LA    R1,4(,R1)           BUMP PROD PTR                                
         BCT   R0,LRL10                                                         
         DC    H'0'                                                             
LRL15    MVC   LPRD,0(R1)                                                       
         SPACE                                                                  
LRL20    OC    MKLKBEST,MKLKBEST   ANY ESTIMATE                                 
         BZ    LRL25                                                            
         SPACE                                                                  
         EDIT  (B1,MKLKBEST),(3,QEST),ALIGN=LEFT                                
         MVC   LEST,QEST           DISPLAY ESTIMATE                             
         SPACE                                                                  
LRL25    MVC   LMKTLNM,MKLKLNAM    MARKET LIST NAME                             
         SPACE                                                                  
         LA    R2,L'LMKTLST/6                                                   
         LA    R3,LMKTLST                                                       
         SPACE                                                                  
LRL30    SR    R0,R0                                                            
         ICM   R0,3,MKLMKT                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QMKT,DUB                                                         
         MVC   0(4,R3),QMKT                                                     
         SPACE                                                                  
         BAS   RE,NEXTEL                                                        
         BNE   LRL40                                                            
         LA    R3,6(,R3)                                                        
         BCT   R2,LRL30                                                         
         SPACE                                                                  
         SH    R3,=H'6'            BACK TO LAST MARKET PRINTED                  
         XC    0(6,R3),0(R3)                                                    
         MVC   0(3,R3),=C'...'     THERE IS MORE                                
         SPACE                                                                  
LRL40    MVI   NLISTS,16           LONGER LIST (STD=15)                         
         GOTO1 LISTMON             LET CONTROLLER BUILD SCREEN                  
         B     LR10                                                             
         EJECT                                                                  
* FORMAT OFFLINE REPORT HERE                                                    
         SPACE                                                                  
LRR      MVC   P,SPACES                                                         
         SPACE                                                                  
         GOTO1 CLUNPK,DMCB,MKLKCLT,PCLT                                         
         SPACE                                                                  
         CLI   MKLKBPRD,0                                                       
         BE    LRR20                                                            
         SPACE                                                                  
         LA    R0,220              MAX PRDS                                     
         L     R1,ASVCLIST         TABLE OF CLIENT PROD CODES                   
LRR10    CLI   0(R1),C' '          AT END OF TABLE?                             
         BH    *+6                                                              
         DC    H'0'                PROBLEM!!!                                   
         SPACE                                                                  
         CLC   3(1,R1),MKLKBPRD    THIS A VALID PROD CODE                       
         BE    LRR15                                                            
         LA    R1,4(,R1)           BUMP PROD PTR                                
         BCT   R0,LRR10                                                         
         DC    H'0'                                                             
LRR15    MVC   PPROD,0(R1)         MOVE IN PROD                                 
         SPACE                                                                  
LRR20    CLI   MKLKBEST,0          ANY ESTIMATE                                 
         BE    LRR22                NO                                          
         SPACE                                                                  
         EDIT  (B1,MKLKBEST),(3,QEST),ALIGN=LEFT                                
         MVC   PEST,QEST           PRINT ESTIMATE                               
         SPACE                                                                  
LRR22    MVC   PMKTLNM,MKLKLNAM                                                 
         MVC   MKTLNM,MKLKLNAM                                                  
         SPACE                                                                  
LRR25    LA    R2,L'PMKTLST/6                                                   
         LA    R3,PMKTLST                                                       
         SPACE                                                                  
LRR30    SR    R0,R0                                                            
         ICM   R0,3,MKLMKT                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QMKT,DUB                                                         
         MVC   0(4,R3),QMKT                                                     
         SPACE                                                                  
         BAS   RE,NEXTEL                                                        
         BNE   LRR40                                                            
         LA    R3,6(,R3)                                                        
         BCT   R2,LRR30                                                         
         SPACE                                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LRR25                                                            
         SPACE                                                                  
LRR40    GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)     PRINT BLANK LINE                             
         SPACE                                                                  
         B     LR10                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
* READ T1 PROFILE *                                                             
         SPACE                                                                  
GETPRF   NTR1                                                                   
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0T1'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
         MVC   SVT1PR16,SVPROF+15  INSTRUCTIONS BY PRODUCT?                     
         SPACE                                                                  
* READ T0 PROFILE                                                               
         SPACE                                                                  
         MVI   WORK+3,C'0'                                                      
         GOTO1 (RF),(R1),,SVPROF                                                
         MVC   SVT1PR11,SVPROF+10  COPY CODE                                    
         XIT1                                                                   
         EJECT                                                                  
* CLEAR REST OF SCREEN AND SET OFF PROTECT BITS                                 
         SPACE                                                                  
CLR      NTR1                                                                   
         LA    RF,TRATAGH          BLANK REST OF SCREEN                         
         XC    WORK,WORK                                                        
CLR10    CR    R2,RF               AT END OF SCREEN                             
         BNL   EXIT                YES                                          
         ZIC   R0,0(R2)                                                         
         LR    R1,R0                                                            
         SH    R1,=H'9'            GET FLD LEN-1                                
         SPACE                                                                  
         EX    R1,CLRCLC            SEE IF FLD SPACES                           
         BNH   CLR14                                                            
         EX    R1,CLRMVC            MAKE FLD BLANK                              
         OI    6(R2),X'80'                                                      
CLR14    AR    R2,R0                                                            
         B     CLR10                                                            
CLRCLC   CLC   8(0,R2),SPACES                                                   
CLRMVC   MVC   8(0,R2),WORK                                                     
         EJECT                                                                  
         SPACE 3                                                                
* HEADING ROUTINE FOR REPORT *                                                  
         SPACE                                                                  
HDHK     NTR1                                                                   
         MVC   H5+11(1),QMED                                                    
*TEMP    CLC   P,SPACES                                                         
*        BNH   HDHK10                                                           
         MVC   PMKTLNM,MKTLNM                                                   
         B     EXIT                                                             
         SPACE                                                                  
HDHK10   MVI   HDHKSW,1                                                         
         B     EXIT                                                             
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         SPACE 2                                                                
INVPRDER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVPRMSG),INVPRMSG                                     
         B     ERREXIT                                                          
         SPACE 2                                                                
DUPERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'DUPERMSG),DUPERMSG                                     
         B     ERREXIT                                                          
         SPACE 2                                                                
NOMKTERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOMKTMSG),NOMKTMSG                                     
         LA    R2,TRAMKT1H                                                      
ERREXIT  GOTO1 ERREX2                                                           
         DC    H'0'                                                             
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
NOMKTMSG DC    C'* ERROR * NO MARKET CODE ENTERED *'                            
DUPERMSG DC    C'* ERROR * DUPLICATE ENTRY *'                                   
INVPRMSG DC    C'* ERROR * PROD POL NOT ALLOWED *'                              
         SPACE 3                                                                
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H2,3,C'--------------'                                           
         SSPEC H3,3,PAGE                                                        
         SSPEC H5,3,C'MEDIA'                                                    
         SSPEC H1,30,C'M A R K E T   L I S T'                                   
         SSPEC H2,30,C'---------------------'                                   
         SSPEC H1,73,AGYNAME                                                    
         SSPEC H2,73,AGYADD                                                     
         SSPEC H4,73,REPORT                                                     
         SSPEC H4,85,RUN                                                        
         SSPEC H5,73,REQUESTOR                                                  
         SSPEC H8,3,C'CLIENT'                                                   
         SSPEC H9,3,C'------'                                                   
         SSPEC H8,11,C'PROD'                                                    
         SSPEC H9,11,C'----'                                                    
         SSPEC H8,17,C'EST'                                                     
         SSPEC H9,17,C'---'                                                     
         SSPEC H8,23,C'MLNAME'                                                  
         SSPEC H9,23,C'------'                                                  
         SSPEC H8,32,C'MARKET LIST '                                            
         SSPEC H9,32,C'--------------------'                                    
         DC    X'00'               END MARKER FOR SSPEC                         
         EJECT                                                                  
         DROP  RB,RC                                                            
         LTORG                                                                  
         EJECT                                                                  
* FIND CLIENT HEADER AND SAVE CLIST                                             
         SPACE                                                                  
         DS    0H                                                               
FCLT     NMOD1 0,**FCLT**                                                       
         SPACE                                                                  
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         SPACE                                                                  
         MVC   BBCLT,BCLT          SAVE                                         
         SPACE                                                                  
* SAVE CURRENT RECORD                                                           
         SPACE                                                                  
FCLT10   L     R0,AIO3                                                          
         L     RE,AIO1                                                          
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
         SPACE                                                                  
         MVC   SVKEY,KEY                                                        
         SPACE                                                                  
         GOTO1 CLUNPK,DMCB,SVBCLT,QCLT                                          
         SPACE                                                                  
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         MVI   FLDH+5,3            DATA LEN                                     
         MVC   FLD(L'QCLT),QCLT    CLIENT                                       
         LA    R2,FLDH                                                          
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         GOTO1 VALICLT                                                          
         MVC   SVBCLT,BCLT                                                      
         SPACE                                                                  
         MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BE    FCLT20                                                           
         CLI   ERROR,SECLOCK       ONLY VALID ERR IS SECURITY LOCK-OU           
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         MVC   KEY,SVKEY                                                        
         MVI   KEY+5,X'FF'         GET NEXT CLIENT                              
         GOTO1 HIGH                                                             
         CLC   KEY(3),KEYSAVE      IF SAME REC TYPE & A/M                       
         BNE   FCLTNE                                                           
         MVC   SVBCLT,KEY+3        SAVE CLIENT                                  
         SPACE                                                                  
* DO GETREC & THEN SAVE REC                                                     
         SPACE                                                                  
         GOTO1 GETREC                                                           
         B     FCLT10                                                           
         SPACE                                                                  
FCLT20   DS    0H                                                               
         SPACE                                                                  
         L     R0,AIO1             MOVE BACK CURRENT RECORD                     
         L     RE,AIO3                                                          
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
         SPACE                                                                  
* READ PROFILES FOR THIS CLT                                                    
         SPACE                                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0T1'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
         MVC   SVT1PR16,SVPROF+15  INSTRUCTIONS BY PRODUCT?                     
         SPACE                                                                  
* READ T0 PROFILE                                                               
         SPACE                                                                  
         MVI   WORK+3,C'0'                                                      
         GOTO1 (RF),(R1),,SVPROF                                                
         MVC   SVT1PR11,SVPROF+10  COPY CODE                                    
         SPACE                                                                  
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY AND DISK ADDR                    
         MVC   AIO,AIO1                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         CR    RB,RB                                                            
         B     FCLTX                                                            
         SPACE                                                                  
FCLTNE   LTR   RB,RB                                                            
FCLTX    MVC   BCLT,BBCLT          RESTORE                                      
         XIT1                                                                   
         DROP  RB,RC                                                            
         EJECT                                                                  
       ++INCLUDE SPTRMKL                                                        
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE SPTRAFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRA4CD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
SPTR51RR DS    A                                                                
FLDH     DS    CL8                                                              
FLD      DS    CL32                                                             
MKTSW    DS    XL1                                                              
SVMKTLN  DS    XL5                                                              
SVBCLT   DS    XL2                                                              
MKTLNM   DS    CL5                                                              
HDHKSW   DS    XL1                                                              
BBCLT    DS    XL2                                                              
         SPACE                                                                  
* OFFLINE REPORT                                                                
         SPACE                                                                  
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL4                                                              
PCLT     DS    CL3                                                              
         DS    CL4                                                              
PPROD    DS    CL3                                                              
         DS    CL3                                                              
PEST     DS    CL2                                                              
         DS    CL3                                                              
PMKTLNM  DS    CL5                                                              
         DS    CL4                                                              
PMKTLST  DS    CL82                                                             
         SPACE 3                                                                
* ONLINE LIST                                                                   
         SPACE                                                                  
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LCLT     DS    CL3                                                              
         DS    CL1                                                              
LPRD     DS    CL3                                                              
         DS    CL1                                                              
LEST     DS    CL3                                                              
         DS    CL1                                                              
LMKTLNM  DS    CL5                                                              
         DS    CL3                                                              
LMKTLST  DS    CL54                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007SPTRA51   02/15/07'                                      
         END                                                                    
