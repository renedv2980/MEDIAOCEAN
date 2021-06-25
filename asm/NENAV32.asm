*          DATA SET NENAV32    AT LEVEL 024 AS OF 04/04/18                      
*PHASE T31832A                                                                  
*INCLUDE GETIDS                                                                 
T31832   TITLE 'NENAV32 - MATCHMAKER - REQUESTS/COMMENTS'                       
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* AKAT SPSUG-713   12/01/17 HONOR RETAIN ORIGINAL CALENDAR TYPE ON MK *         
***********************************************************************         
T31832   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,NENV32**,RR=R4                                                 
         ST    R4,RELO                                                          
         B     COM00                                                            
RELO     DS    A                                                                
*                                                                               
COM00    LR    RC,R1                                                            
         USING WORKD,RC                                                         
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
*                                                                               
         CLI   I2DRFTSW,C'N'                                                    
         BE    COM02                                                            
         GOTOR TSTUPD                                                           
*                                                                               
* TEST FOR I2 COMMENT DATA                                                      
COM02    L     R4,AIO2                                                          
         CLI   I2DRFTSW,C'N'       IF DRAFT, DON'T CHANGE COMMENTS              
         BE    COM50                                                            
         TM    SVCOMFLG,SVCMDELQ   DELETE ALL COMMENTS?                         
         BO    *+14                                                             
         OC    0(16,R4),0(R4)                                                   
         BZ    COM50               NO COMMENT - JUST ADD REQ                    
*                                                                               
X        USING XCOMRECD,KEY                                                     
*                                                                               
         XC    KEY,KEY                                                          
         MVC   X.COMI2K,=X'0D0C'                                                
         MVC   X.COMI2KAM,BAGYMD                                                
         MVI   X.COMI2KTY,C'I'                                                  
         MVC   X.COMI2KCL,BCLT                                                  
         MVC   X.COMI2KPR,QPRD                                                  
         MVC   X.COMI2KES,I2ESTB                                                
         MVC   X.COMI2KP2,QPRD2                                                 
         CLC   BEST,BEST2                                                       
         BE    *+10                                                             
         MVC   X.COMI2KE2,BEST2                                                 
         MVC   X.COMI2KST,BSTAP                                                 
         MVC   X.COMI2KYM,BMOS                                                  
         MVI   XSPT,C'Y'                                                        
         GOTO1 AIOCALL,DMCB,XSP+DIR+HIGH+UPDATE                                 
*                                                                               
         USING XCOMRECD,R8                                                      
         L     R8,AIO1                                                          
         ST    R8,AIO                                                           
*                                                                               
         CLC   KEY(32),KEYSAVE                                                  
         BE    COM10                                                            
         DROP  X                                                                
* BUILD NEW RECORD                                                              
*                                                                               
         XC    0(256,R8),0(R8)                                                  
         MVC   0(32,R8),KEYSAVE                                                 
         MVC   32(2,R8),=AL2(32+10+12)                                          
         MVI   XCOMEL,1                                                         
         MVI   XCOMEL+1,12                                                      
         GOTO1 VDATCON,DMCB,(5,0),(3,XCOMEL+2)  CREATED                         
         B     COM12                                                            
*                                                                               
COM10    MVI   XSPT,C'Y'                                                        
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 AIOCALL,DMCB,XSP+FIL+GET+UPDATE,AIO                              
*                                                                               
COM12    GOTO1 VDATCON,DMCB,(5,0),(3,XCOMEL+5)  ACTIVITY DATE                   
         SPACE 1                                                                
*=============================================================*                 
* REMOVE ALL 05 ELEMENTS                                                        
*=============================================================*                 
         SPACE 1                                                                
COM20    GOTO1 VHELLO,DMCB,(C'D',=C'XSPFIL'),(5,(R8)),0,0                       
         TM    SVCOMFLG,SVCMDELQ   DELETE ALL COMMENTS?                         
         BO    COM30                                                            
*                                                                               
         L     R6,AIO2             POINT TO COMMENT DATA                        
*                                                                               
COM22    CLI   0(R6),0                                                          
         BE    COM30                                                            
*                                                                               
         LA    RE,BLOCK                                                         
         LA    RF,300                                                           
         XCEF                                                                   
         MVI   BLOCK,X'05'                                                      
         ZIC   R1,0(R6)                                                         
         AHI   R1,1                                                             
         STC   R1,BLOCK+1          SET ELEMENT LENGTH                           
         SHI   R1,3                SET FOR EX                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BLOCK+2(0),1(R6)                                                 
         OC    BLOCK+2(80),SPACES  MAKE SURE UPPERCASE                          
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',=C'XSPFIL'),(R8),BLOCK,=C'ADD=CODE'            
*                                                                               
         SR    R0,R0                                                            
         IC    R0,0(R6)                                                         
         AR    R6,R0                                                            
         B     COM22                                                            
*                                                                               
COM30    DS    0H                                                               
         MVI   XSPT,C'Y'                                                        
         CLC   KEY(32),KEYSAVE     DID WE FIND THE RECORD                       
         BE    COM35                                                            
         GOTO1 AIOCALL,DMCB,XSP+FIL+ADDREC,AIO                                  
         B     COM50                                                            
COM35    GOTO1 AIOCALL,DMCB,XSP+FIL+PUT,AIO                                     
         EJECT                                                                  
*===============================================================*               
* BUILD A REQUEST                                               *               
*===============================================================*               
         SPACE  1                                                               
COM50    XC    IDNUM,IDNUM                                                      
         CLC   QDEST,SPACES                                                     
         BNH   *+8                                                              
         BAS   RE,GETDEST                                                       
         BAS   RE,READSTA                                                       
         BAS   RE,GETI2PR                                                       
*                                                                               
         MVI   XSPT,C'N'                                                        
         LA    RE,BLOCK                                                         
         LA    RF,300                                                           
         XCEF                                                                   
         MVC   BLOCK+26(80),SPACES                                              
         MVC   BLOCK+106(80),SPACES                                             
*                                                                               
Z        USING ZRECD,BLOCK                                                      
*                                                                               
         MVC   Z.ZPROG,=C'I2'                                                   
         MVC   Z.ZAGY,QAGY                                                      
         MVI   Z.ZMED,C'N'                                                      
         MVC   Z.ZCLT,QCLT                                                      
         MVC   Z.ZPRD,QPRD                                                      
         CLI   I2PRDGRP,0                                                       
         BE    COM52                                                            
         MVC   Z.ZPGR,I2PRDGRP      PRODUCT GROUP INFO                          
         MVC   Z.ZPRD,I2PRDGRP+1    PRODUCT GROUP INFO                          
COM52    MVC   Z.ZSTA(4),BNET                                                   
         MVC   Z.ZEST,I2EST                                                     
         CLC   I2EST,=CL3'000'                                                  
         BNE   *+10                                                             
         MVC   Z.ZEST,=CL3'NO '                                                 
         OC    I2ESTFLT,I2ESTFLT                                                
         BZ    *+10                                                             
         MVC   Z.ZESTEND,I2ESTFLT                                               
         MVC   Z.ZPRD2,QPRD2       PIGGYBACK PARTNER                            
         MVC   Z.ZSTART(4),SVEESDAT   END YYMM                                  
         MVC   Z.ZUESTOR,QUESTOR                                                
         MVI   Z.ZOPT5,C'Y'        TELL I2 TO POST AFFIDS                       
         CLI   I2DRFTSW,C'N'                                                    
         BNE   *+8                                                              
         MVI   Z.ZOPT5,C'N'        TELL I2 TO RUN AS DRAFT                      
         OC    Z.ZAREA,SPACES      MAKE REQUEST UPPERCASE/NO NULLS              
*                                                                               
         CLI   I2PRDGRP,0                                                       
         BE    *+8                                                              
         NI    Z.ZPGR,X'BF'         REMOVE 40 BIT MAKE LOWER CASE               
*                                                                               
         MVC   Z.Z2MATCH,I2INTREQ                                               
         OI    Z.Z2MATCH,X'40'                                                  
         MVC   Z.Z2CALAND,I2CALREQ  RETAIN CALENDAR TYPE                        
         CLI   SVMKPRFL+12,C'Y'     RETAIN ORIGINAL CALENDAR TYPE?              
         BE    *+10                 YES                                         
         MVC   Z.Z2CALAND,SVI2PRFL+9  GET CALENDAR TYPE FROM PROFILE            
*                                                                               
         MVI   Z.ZCONTREQ,C'*'     MORE DATA IN QAREA2                          
         OI    Z.ZCTL+15,X'10'      SET HAS 2ND REQUEST                         
*                                                                               
         MVC   Z.ZCOMPARE,MEDTYPE                                               
         DROP  Z                                                                
*                                                                               
         CLI   QWHEN,C'S'          TEST SOON REQUEST                            
         BE    COM60                                                            
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'REQUEST',BLOCK,BLOCK                  
*                                                                               
         MVC   WORK(9),=C'REQUESTED'                                            
         B     COMX                                                             
         EJECT                                                                  
*===========================================================*                   
* TRY TO ADD A SOON REQUEST                                 *                   
*===========================================================*                   
         SPACE 1                                                                
COM60    DS    0H                                                               
         LA    R2,WORK2                                                         
         USING SPOOK,R2                                                         
         XC    0(SPOOKL,R2),0(R2)  BUILD SPOOK BLOCK                            
*                                                                               
         MVC   SPOOKUID,TWAUSRID   CONNECT ID                                   
         OC    IDNUM,IDNUM         TEST OVERRIDE                                
         BZ    *+10                                                             
         MVC   SPOOKUID,IDNUM                                                   
*                                                                               
         MVC   SPOOKAGY,TWAAGY     TWO CHARACTER ID CODE                        
         MVC   SPOOKDID,QUESTOR    USER INITIALS (ID)                           
         OC    SPOOKDID,SPACES                                                  
         CLI   SPOOKDID+2,C' '                                                  
         BH    *+8                                                              
         MVI   SPOOKDID+2,C'*'                                                  
         CLI   SPOOKDID+1,C' '                                                  
         BH    *+8                                                              
         MVI   SPOOKDID+1,C'*'                                                  
*                                                                               
         MVC   SPOOKSYS,=C'NE'     NET SYSTEM                                   
         MVC   SPOOKEOD,=C'I2'                                                  
         MVC   SPOOKJCL,=C'I2'                                                  
         MVI   SPOOKWEN,5          SET SOON STATUS/UPDATIVE SOON                
*                                                                               
         CLI   I2DRFTSW,C'N'                                                    
         BE    *+8                  DON'T LOCK IF DRAFT                         
         BRAS  RE,ADDLOCKS                                                      
*                                                                               
         L     RE,ACOMFACS         A(COMFACS)                                   
         USING COMFACSD,RE                                                      
         L     RF,CREQTWA                                                       
         DROP  RE                                                               
*                                                                               
         XC    DMCB(24),DMCB                                                    
         GOTO1 (RF),DMCB,(5,(RA)),BLOCK,VDATAMGR,ACOMFACS,(R2)                  
*                                                                               
         L     RE,8(R1)            GET A(PRTQUE) KEY                            
         SR    R0,R0                                                            
         ICM   R0,3,6(RE)                                                       
         EDIT  (R0),(4,FULL),ALIGN=LEFT                                         
* BUILD SOON ID STRING FOR HABES                                                
         MVC   WORK(3),SPOOKDID                                                 
         MVI   WORK+3,C','                                                      
         MVC   WORK+4(4),FULL                                                   
         MVI   WORK+8,C' '                                                      
*                                                                               
COMX     LHI   R1,X'53'                                                         
         BRAS  RE,SENDH                                                         
*                                                                               
         LA    R1,4                                                             
         LA    R4,WORK                                                          
         BRAS  RE,SENDD                                                         
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO TEST IF FILE IS UPDATABLE                                *         
***********************************************************************         
                                                                                
TSTUPD   L     RF,ACOMFACS                                                      
         L     RF,CXTRAINF-COMFACSD(RF)                                         
         USING XTRAINFD,RF                                                      
         TM    XIFLAG2,XICTUEN     TEST CONNECTED WITH U=N OR                   
         JNZ   TSTUPD02            READ-ONLY                                    
         TM    XIFLAG1,XIROSYS+XIROMODE+XIWRONGF                                
         BZR   RE                                                               
         DROP  RF                                                               
                                                                                
TSTUPD02 MVC   ERROR,NOUPDTS       SEND NO UPDATES ERROR                        
         GOTOR SENDMSG                                                          
         EJECT                                                                  
NOUPDTS  DC    AL2(847)            ERROR - CAN'T UPDATE - READ-ONLY             
*=================================================================*             
* READ I2 PROFILE                                                               
*=================================================================*             
         SPACE 1                                                                
GETI2PR  NTR1                                                                   
         MVC   WORK(4),=C'SI2Z'                                                 
         NI    WORK,X'BF'          CHANGE 'S' TO LOWER CASE                     
         MVC   WORK+4(2),QAGY                                                   
         MVI   WORK+6,C'N'                                                      
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVOFFC                                                
         L     RF,ACOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         LA    R5,SVI2ZPRF                                                      
         GOTO1 (RF),DMCB,WORK,(R5),VDATAMGR                                     
         CLI   SVI2ZPRF+6,C'N'      READ BY SUBMEDIA                            
         BE    *+10                                                             
         MVC   WORK+6(1),MEDTYPE    MOVE SUBMEDIA INTO THE KEY                  
         MVC   WORK(4),=C'S0I2'                                                 
         LA    R5,SVI2PRFL                                                      
         GOTO1 (RF),DMCB,WORK,(R5),VDATAMGR                                     
*                                                                               
         XC    WORK,WORK            CLEAR WORK FOR MK PROFILE READ              
         MVC   WORK(4),=C'S0MK'     READ THE MK PROFILE AT AGENCY LEVEL         
         MVC   WORK+4(2),QAGY       AGENCY LEVEL MK PROFILE                     
         LA    R5,SVMKPRFL          SAVED MK PROFILE                            
*                                                                               
         GOTO1 (RF),DMCB,(X'90',WORK),(R5),VDATAMGR                             
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*=================================================================*             
* READ MY ID RECORD FROM CTFILE AND CALL GETIDS TO VALIDATE DEST  *             
*=================================================================*             
         SPACE 1                                                                
GETDEST  NTR1                                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+23(2),TWAUSRID                                               
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,AIO2                     
*                                                                               
         L     R6,AIO2                                                          
         USING CTIREC,R6                                                        
         CLC   KEY(25),0(R6)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
* NOW CHECK THAT DESTINATION IS VALID                                           
         XC    WORK,WORK                                                        
         MVC   WORK(8),QDEST                                                    
         OC    WORK(10),SPACES                                                  
         GOTO1 =V(GETIDS),DMCB,(C'D',AIO2),0,(C'A',VDATAMGR),WORK,     X        
               RR=RELO                                                          
*                                                                               
         CLI   DMCB+12,0           TEST ANY MATCH (EXACT OR ALL <> 0)           
         BE    GETIDERR            NO - ERROR                                   
*                                                                               
         L     RE,DMCB+4           GET A(OUTPUT BLOCK)                          
         MVC   IDNUM,10(RE)        SAVE DEST IDNUM                              
         B     EXIT                                                             
*                                                                               
GETIDERR MVI   ERROR+1,BADUSRID                                                 
         XC    ERRORMSG,ERRORMSG                                                
         GOTO1 SENDMSG                                                          
         DROP  R6                                                               
         EJECT                                                                  
*=================================================================*             
* ON ENTRY R1 CONTAINS HEADER CODE                                *             
*=================================================================*             
         SPACE 1                                                                
SENDH    LR    R0,RE                                                            
         GOTO1 GETHDR              GET HEADER ADDRESS                           
         GOTO1 ASETELEM,DMCB,AFABLK,HDRADDR,0,0                                 
         SR    R5,R5               CLEAR LENGTH OVERRIDE                        
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
*===============================================================*               
* PARMS ARE FABLK,MAP_TABLE_ENTRY,A(DATA),OVRD_LEN              *               
* ON ENTRY R1 CONTAINS DATA ITEM NUMBER WITHIN CURRENT ELEMENT  *               
*===============================================================*               
         SPACE 1                                                                
SENDD    LR    R0,RE                                                            
         GOTO1 GETDATA             GET DATA ITEM                                
         GOTO1 AADDDATA,DMCB,AFABLK,DATADDR,(R4),(R5)                           
         SR    R5,R5               CLEAR OVERRIDE LENGTH                        
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*  READ STATION RECORD CHECK POSTING TYPE                                       
*                                                                               
READSTA  NTR1                                                                   
         MVC   AIO,AIO3                                                         
         LA    R5,KEY                                                           
         USING STAREC,R5                                                        
         XC    KEY,KEY                                                          
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'N'                                                     
         MVC   STAKCALL(4),BNET                                                 
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,QAGY                                                     
         MVC   STAKCLT(6),=6X'F0'                                               
*                                                                               
         GOTO1 AIOCALL,DMCB,STA+FIL+HIGH,AIO3                                   
         CLC   KEY(9),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIO3                                                          
         MVC   MEDTYPE,STYPE                                                    
         XIT1                                                                   
         DROP  R5                                                               
*===============================================================*               
* ADD LOCKS FOR UPDATIVE SOON REQUESTS                          *               
*===============================================================*               
         SPACE 1                                                                
ADDLOCKS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 VGETFACT,DMCB,(2,0)                                              
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   SENUMBER,FASYS                                                   
*                                                                               
* TEST NV LOCK - IF NOT AVAILABLE, STOP                                         
ADDLK2   BAS   RE,BLDNVLK                                                       
         L     RF,ACOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKTESTQ',LKBLOCK),ACOMFACS                           
         CLI   4(R1),2                                                          
         BE    ADDLK2                                                           
         CLI   4(R1),0                                                          
         BNE   ADDLKERR                                                         
* ADD BUY LOCK                                                                  
         BAS   RE,BLDUNLK                                                       
*                                                                               
ADDLK4   L     RF,ACOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKLOCKQ',LKBLOCK),ACOMFACS                           
         CLI   4(R1),2                                                          
         BE    ADDLK4                                                           
         CLI   4(R1),0                                                          
         BNE   ADDLKERR                                                         
*                                                                               
         BAS   RE,BLDNVLK                                                       
*                                                                               
ADDLK6   L     RF,ACOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKLOCKQ',LKBLOCK),ACOMFACS                           
         CLI   4(R1),2                                                          
         BE    ADDLK6                                                           
         CLI   4(R1),0                                                          
         BNE   ADDLKERR                                                         
*                                                                               
ADDLKX   XIT1                                                                   
*                                                                               
ADDLKERR MVI   ERROR+1,CLLOKERR                                                 
         XC    ERRORMSG,ERRORMSG                                                
         GOTO1 SENDMSG                                                          
*                                                                               
L        USING LKKEYD,LKBLOCK                                                   
*                                                                               
BLDUNLK  DS    0H                                                               
         XC    L.LOCKEY,L.LOCKEY                                                
         MVC   L.LOCKSE,SENUMBER                                                
         MVC   L.LOCKAGY,QAGY                                                   
         MVC   L.LOCKRTY,=C'UN'    INVOICE RECORDS                              
         MVC   L.LKNVCLT,QCLT                                                   
         OC    L.LKNVCLT,SPACES                                                 
         MVC   L.LKNVSTA,BNET                                                   
         BR    RE                                                               
         DROP  L                                                                
*                                                                               
L        USING LKKEYD,LKBLOCK                                                   
*                                                                               
BLDNVLK  XC    L.LOCKEY,L.LOCKEY                                                
         MVC   L.LOCKSE,SENUMBER                                                
         MVC   L.LOCKAGY,QAGY                                                   
         MVC   L.LOCKRTY,=C'NV'    BUY RECORDS                                  
         MVI   L.LKBUMED,C'N'                                                   
         MVC   L.LKBUCLT,QCLT                                                   
         OC    L.LKBUCLT,SPACES                                                 
         MVC   L.LKBUSTA,BNET                                                   
         MVI   L.LKBUSTA+4,C'N'    SET BAND IF NOT SET                          
         BR    RE                                                               
         DROP  L                                                                
         LTORG                                                                  
       ++INCLUDE FALOCKUPD                                                      
       ++INCLUDE FAXTRAINF                                                      
LKKEYD   DSECT                                                                  
         ORG   LOCKKEY                                                          
LKBUMED  DS    XL1                                                              
LKBUCLT  DS    XL3                                                              
LKBUSTA  DS    XL5                                                              
*                                                                               
         ORG   LOCKKEY                                                          
LKNVCLT  DS    XL3                                                              
LKNVSTA  DS    XL4                                                              
LKNVSPR  DS    XL3                                                              
         EJECT                                                                  
ZRECD    DSECT                                                                  
ZCTL     DS    XL26                                                             
ZAREA    DS    0CL80   COLUMN                                                   
ZPROG    DS    0CL2    ------                                                   
ZCODE    DS    CL2        1        PROGRAM CODE                                 
ZAGY     DS    CL2        3        AGENCY CODE                                  
ZMED     DS    CL1        5        MEDIA CODE (R/T)                             
ZCLT     DS    CL3        6        CLIENT CODE                                  
ZPGR     DS    CL1        9        PROCESS BY DIVISION                          
ZMGR     DS    CL1       10        PROCESS BY DISTRICT                          
ZCLOFFC  DS    CL1       11        CLIENT OFFICE FILTER                         
ZBYID    EQU   ZCLOFFC             C'Y' IF BUYS PROCESSED BY ID                 
ZPRD     DS    CL3       12        PRODUCT MNEMONIC                             
ZMKT     DS    CL4       15        MARKET NUMBER                                
ZSTA     DS    CL5       19        STATION CALL LETTERS                         
ZEST     DS    CL3       24        ESTIMATE NUMBER                              
ZESTEND  DS    CL3       27        LAST NUMBER IN ESTIMATE GROUP                
         DS    CL1       30        Y=DEMO OVERRIDE ACTIVE                       
ZCONTREQ DS    CL1       31        C'*' ==> DATA IN QAREA2                      
ZSTAUTO  DS    CL3       32        AUTO REQUEST START DATE                      
ZENDAUTO DS    CL3       35        AUTO REQUEST END DATE                        
         ORG   *-3                                                              
ZPRD2    DS    CL3       35        PIGGYBACK PARTNER                            
ZSTART   DS    CL6       38        REQUEST START DATE                           
ZEND     DS    0CL6      44        REQUEST END DATE                             
ZTODAY   DS    CL6       44                                                     
ZBOOK1   DS    CL4       50        RATING BOOK (YYMM) GOAL/ESTD DATA            
ZHUT1    DS    CL2       54        HUT ADJUSTMENT MONTH                         
MRERATE  DS    CL1       56        RERATE TYPE                                  
ZCOMPARE DS    CL1       57        DATA COMPARE OPTION                          
ZAFFIL   DS    CL1       58        AFFILIATION FILTER                           
ZPRGTYPE DS    CL1       59        PROGRAM TYPE FILTER                          
ZDPTDET  DS    CL1       60        DAYPART DETAIL CONTROL                       
*                                  A=SHOW FULL DETAIL (DEFAULT)                 
*                                  B=SUPPRESS SPOT-LENGTH                       
*                                  C=SUPPRESS SPOT-LENGTH & DAY-PART            
ZDPTMENU DS    CL1       61        DAYPART MENU OVERRIDE                        
ZOPT1    DS    CL1       62        OPTION 1                                     
ZOPT2    DS    CL1       63        OPTION 2                                     
ZOPT3    DS    CL1       64        OPTION 3                                     
ZOPT4    DS    CL1       65        OPTION 4                                     
ZOPT5    DS    CL1       66        OPTION 5                                     
ZGRP     DS    CL2       67        GROUP                                        
ZFILTER  EQU   QGRP                FILTER TYPE/VALUE                            
ZUESTOR  DS    CL12      69        REQUESTOR NAME                               
*                                                                               
Z2AREA   DS    0CL80                                                            
         DS    CL21                                                             
Z2CALAND DS    CL1       21         BROADCAST/CALANDER MONTH                    
Z2MATCH  DS    CL1       22         MATCH INT AND ACTUAL/ OR JUST INT           
         DS    CL58                                                             
         EJECT                                                                  
         ORG   ZAREA+57                                                         
ZCMRCL   DS    CL8       58        COMMERCIAL FILTER                            
         ORG   ZAREA+29                                                         
ZREP     DS    CL3       30        DISCREPANCY REP                              
         ORG                                                                    
         PRINT OFF                                                              
       ++INCLUDE NENAVWRK                                                       
WORKD    DSECT                                                                  
         ORG   OVWORK                                                           
IDNUM    DS    H                                                                
LKBLOCK  DS    XL25                                                             
SENUMBER DS    XL1                                                              
MEDTYPE  DS    CL1                                                              
SVI2ZPRF DS    CL16                                                             
SVI2PRFL DS    CL16                                                             
SVMKPRFL DS    CL16                                                             
         ORG                                                                    
       ++INCLUDE DDSPOOK                                                        
XCOMRECD DSECT                                                                  
       ++INCLUDE SPGENXCOM                                                      
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE FAFACTS                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024NENAV32   04/04/18'                                      
         END                                                                    
