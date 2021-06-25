*          DATA SET SPBUYVAL   AT LEVEL 009 AS OF 01/11/18                      
*PROCESS USING(WARN(15))                                                        
*PHASE T00A2AA                                                                  
         SPACE 2                                                                
***********************************************************************         
* PARAM1 BYTE    0   = CPROF+0                                                  
*        BYTE(S) 1-3 = A(BUY RECORD VALIDATION BLOCK / SPBUYVALD)               
* PARAM2 BYTE    0   = Y/N - SHOW THE ERROR ON SCREEN                           
*        BYTE(S) 1-3 = A(MESSAGE FLD HEADER)                                    
***********************************************************************         
         TITLE 'SPBUYVAL - SPOT SYSTEM BUY RECORD VALIDATION'                   
SPBUYVAL CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,SPBUYVAL,CLEAR=YES                                   
         USING (WORKD,WORKX),RC    RC=A(W/S)                                    
         ST    RD,SAVERD           SAVE RD FOR ERROR EXIT                       
         ST    R1,APARM                                                         
         MVC   SVCPROF1,0(R1)      SAVE CPROF FIELD 1 (BRAND/POL TRNDS)         
         L     R3,0(R1)            SAVE OFF THE SPBUYVLD BLOCK                  
         USING SPBUYVLD,R3                                                      
*                                                                               
         CLI   4(R1),C'Y'          DO WE HAVE TO SAVE?                          
         BNE   MAIN005              - NOPE                                      
         MVC   AERRMSG,4(R1)                                                    
         MVC   VMSUNPK,8(R1)                                                    
         OI    MISCFLG1,SHOWERR    WE'LL SHOW THE ERROR MESSAGE                 
*                                                                               
MAIN005  BRAS  RE,INIT             INITIALIZE SOME VARIABLES                    
*                                                                               
         L     R4,SPBYAREC                                                      
B        USING BUYREC,R4                                                        
*                                                                               
         CLI   B.BUYKAM,X'10'      DO WE HAVE A BUY RECORD?                     
         BH    *+12                 - YUP, WE DO                                
         MVI   ERROR,NOTBUY         - NOPE                                      
         B     *+8                    GET OUT OF HERE                           
         BRAS  RE,CHKELEMS         CHECK FOR OTHER ERRORS                       
*                                                                               
         MVC   SPBYERR,ERROR                                                    
         MVC   SPBYCKSM,SUM                                                     
         CLI   ERROR,0             SET CC ON EXIT                               
         B     EXIT                                                             
*                                                                               
NEQEXIT  CR    RB,RD                                                            
         J     *+6                                                              
EQEXIT   CR    RB,RB                                                            
EXIT     XIT1                                                                   
***********************************************************************         
*        INIT  -  INITIALIZE VARIABLES                                *         
***********************************************************************         
INIT     NTR1  BASE=*,LABEL=*                                                   
         ICM   RF,15,SPBYAFAC                                                   
         BNZ   *+6                                                              
         DC    H'0'                DIE IF THERE IS NO COMFACS!                  
         ST    RF,VCOMFACS                                                      
         USING COMFACSD,RF         RF=A(COMFACS)                                
         MVC   VDATCON,CDATCON                                                  
         J     EXIT                                                             
         DROP  RF                                                               
***********************************************************************         
*        CHKELEMS  -  MAKE SURE ELEMENTS ARE IN THE RIGHT ORDER       *         
***********************************************************************         
CHKELEMS NTR1  BASE=*,LABEL=*                                                   
         LA    R6,B.BDELEM         POINTS TO THE FIRST ELEMENT                  
         CLI   0(R6),BDCODEQ       X'01' BUY DESCRIPTION ELEMENT?               
         BNE   NOTSYNC             ELEMENT OUT OF SYNC                          
         BRAS  RE,CHK01EL                                                       
         BNE   CHKELERR                                                         
*                                                                               
         BRAS  RE,CHKLMNXT                                                      
         BNE   CHKELX              WE'RE DONE                                   
*                                                                               
         CLI   0(R6),X'02'                                                      
         BNE   NOTSYNC             ELEMENTS OUT OF SYNC                         
*                                                                               
CHKEL02  DS    0H                                                               
         BRAS  RE,CHKLMNXT                                                      
         BNE   CHKELX              WE'RE DONE                                   
*                                                                               
         CLI   0(R6),X'03'                                                      
         BE    CHKEL03                                                          
         CLI   0(R6),X'22'         POST BUY DEMO ORIGINATING OKAY               
         BNE   CHKEL0B2            ELEMENTS OUT OF SYNC                         
         TM    MISCFLG1,HAVE22     DO WE ALREADY HAVE IT?                       
         BO    NOTSYNC              - YUP                                       
         OI    MISCFLG1,HAVE22                                                  
         B     CHKEL02                                                          
*                                                                               
CHKEL03  DS    0H                                                               
         BRAS  RE,CHKLMNXT                                                      
         BNE   CHKELX              WE'RE DONE                                   
*                                                                               
         CLI   0(R6),X'03'                                                      
         BNE   CHKEL03C                                                         
         NI    MISCFLG1,X'FF'-HAVE23   TAKE OFF THE FLAG                        
         B     CHKEL03                                                          
*                                                                               
CHKEL03C CLI   0(R6),X'23'         POST BUY DEMO ORIGINATING OKAY               
         BNE   CHKEL0B2                                                         
         TM    MISCFLG1,HAVE23     DO WE ALREADY HAVE IT?                       
         BO    NOTSYNC              - YUP                                       
         OI    MISCFLG1,HAVE23                                                  
         B     CHKEL03                                                          
*                                                                               
CHKEL0B  DS    0H                                                               
         BRAS  RE,CHKLMNXT                                                      
         BNE   CHKELSPL                                                         
*                                                                               
CHKEL0B2 CLI   0(R6),X'0B'         DO WE HAVE A X'0B'                           
         BE    *+12                                                             
         CLI   0(R6),X'0C'         DO WE HAVE A X'0C'                           
         BNE   CHKEL0B                                                          
         BRAS  RE,CHK0B0C                                                       
         CLI   ERROR,0             ANY ERROR?                                   
         BE    CHKEL0B              - NOPE, CONTINUE                            
         B     CHKELERR                                                         
*                                                                               
CHKELSPL LA    R6,B.BDELEM         POINTS TO THE FIRST ELEMENT                  
         BRAS  RE,CHKLMNXT                                                      
         BNE   CHKEL68             WE'RE DONE                                   
         CLI   0(R6),NDCSPLQ       X'03'                                        
         BNE   *-12                                                             
         BRAS  RE,CHKSPILL                                                      
         BNE   ERREND                                                           
*                                                                               
CHKEL68  LA    R6,B.BDELEM         POINTS TO THE FIRST ELEMENT                  
         BRAS  RE,CHKLMNXT                                                      
         BNE   CHKELX              WE'RE DONE                                   
         CLI   0(R6),NTWKCODQ      X'68'                                        
         BNE   *-12                                                             
         BRAS  RE,CHK68                                                         
         BNE   ERREND                                                           
*                                                                               
CHKELERR TM    MISCFLG1,SHOWERR                                                 
         BZ    CHKELX                                                           
         MVC   BYTE,ERROR          SAVE OFF ERROR BYTE                          
         XC    DUB,DUB                                                          
         MVC   DUB+2(3),B.BUYKSTAC                                              
         OC    VMSUNPK,VMSUNPK     OPTIONAL                                     
         BZ    CHKEL0B3             - SKIP IT, DON'T NEED                       
*                                                                               
         GOTOR VMSUNPK,DMCB,(X'80',DUB),WORK,WORK+4                             
*                                                                               
CHKEL0B3 L     RF,AERRMSG          THIS IS A HEADER                             
         OI    6(RF),X'80'         TRANSMIT                                     
         XC    8(60,RF),8(RF)                                                   
         MVC   8(L'BUYVLERR,RF),BUYVLERR                                        
         OC    VMSUNPK,VMSUNPK     OPTIONAL                                     
         BZ    *+10                 - SKIP IT                                   
         MVC   8(5,RF),WORK+4                                                   
         CLI   B.BUYKBUY,X'FF'                                                  
         BNL   CHKEL0B5                                                         
         LLC   R1,B.BUYKBUY                                                     
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  8+14(3,RF),DUB+6(2)                                              
         B     CHKELX               - YUP, WE'RE DONE                           
*                                                                               
CHKEL0B5 DS    0H                                                               
         LA    RE,BUYVLTAB                                                      
         CLI   0(RE),X'FF'         ARE WE DONE?                                 
         BE    CHKERRX                                                          
         CLC   BYTE,0(RE)                                                       
         BE    CHKEL0B7                                                         
         LA    RE,L'BUYVLTAB(RE)   43 BYTES                                     
         B     CHKEL0B5                                                         
*                                                                               
CHKEL0B7 DS    0H                                                               
         MVC   8+18(42,RF),1(RE)   MOVE IN THE MESSAGE                          
*                                                                               
CHKERRX  DS    0H                                                               
         B     CHKELX              DO YOUR OWN ABEND!!                          
*                                                                               
BUYVLERR DC    C'      BUYLINE     HAS ERRORS!'                                 
BUYVLTAB DS    0CL43                                                            
         DC    X'01',CL42'IS NOT A BUY RECORD'                                  
         DC    X'02',CL42'HAS ELEMENTS OUT OF SYNC'                             
         DC    X'03',CL42'HAS BAD ELEMENT STRUCTURE'                            
         DC    X'04',CL42'HAS SPOT(S) OUTSIDE THE PERIOD'                       
         DC    X'05',CL42'HAS SPOT DATES OUT OF SEQUENCE'                       
         DC    X'06',CL42'HAS PAID SPOT IN DELETED RECORD'                      
         DC    X'07',CL42'HAS NO ALLOCATED PRODUCT FOR SPOT'                    
         DC    X'08',CL42'TIME DOES NOT ADD UP FOR SPOTS'                       
         DC    X'09',CL42'HAS OTO ERROR'                                        
         DC    X'0A',CL42'POL BUY MUST NOT HAVE TIME'                           
         DC    X'0B',CL42'BAD SPOT LENGTH'                                      
         DC    X'0C',CL42'BAD START/END DAY'                                    
         DC    X'0D',CL42'BUY START/END DATES ARE EMPTY'                        
         DC    X'0E',CL42'START DAY IS INCORRECT'                               
         DC    X'0F',CL42'BAD DAY'                                              
         DC    X'FF'                                                            
*                                                                               
CHKELX   DS    0H                                                               
         J     EXIT                                                             
                                                                                
CHKLMNXT DS    0H                  NEXT ELEMENT                                 
         MVC   SAVEELEM,0(R6)      SAVE OFF LAST ELEMENT CODE                   
         ST    R6,LASTELEM         SAVE OFF LAST ELEMENT ADDRESS                
*                                                                               
         LLC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         BZ    NOLENGTH                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0             DO WE HAVE SOMETHING?                        
         BE    CHKLMNO              - NO, WE DON'T                              
*                                                                               
CHKLMYES DS    0H                                                               
         CR    RE,RE                                                            
         B     CHKLMNX                                                          
*                                                                               
CHKLMNO  DS    0H                                                               
         CR    RB,RE                                                            
CHKLMNX  BR    RE                                                               
*                                                                               
NOLENGTH MVI   ERROR,ELEMERR       ELEMENT HAS NO LENGTH                        
         B     ERREND                                                           
*                                                                               
NOTSYNC  MVI   ERROR,OUTSYNC       OUT OF SYNC ELEMENTS                         
ERREND   DS    0H                                                               
         STCM  R6,15,SPBYELEM       - NOPE, SAVE THE ADDRESS                    
         J     CHKELX                                                           
*                                                                               
***********************************************************************         
*        CHK01EL  -  CHECKING 01                                                
***********************************************************************         
CHK01EL  NTR1  BASE=*,LABEL=*                                                   
         MVC   SAVBDSEC,B.BDSEC                                                 
*                                                                               
         MVC   SVBDMSPR,B.BDMASPRD                                              
*                                                                               
C01E010  CLI   B.BDSEDAY,0                                                      
         BNE   C01E020                                                          
         MVI   ERROR,BADSEDAY                                                   
         B     CHK01NEQ                                                         
*                                                                               
C01E020  CLI   B.BDSEC,0                                                        
         BNE   C01E030                                                          
         MVI   ERROR,BADSLN                                                     
         B     CHK01NEQ                                                         
*                                                                               
C01E030  CLI   B.BDDAY,0                                                        
         BNE   C01E040                                                          
         MVI   ERROR,BADBDDAY                                                   
         B     CHK01NEQ                                                         
*                                                                               
C01E040  OC    B.BDSTART(6),B.BDSTART                                           
         BNZ   C01E050                                                          
         MVI   ERROR,NODATES                                                    
         B     CHK01NEQ                                                         
*                                                                               
C01E050  CLI   B.BDXFRAGY,0                                                     
         BE    C01E060                                                          
         MVI   ERROR,BADXFRAG                                                   
         B     CHK01NEQ                                                         
*                                                                               
C01E060  CLI   B.BDTIME,0          TEST PIGGYBACK                               
         BE    C01E070              - NOPE                                      
         CLI   B.BUYKPRD,X'FF'                                                  
         BNE   C01E070                                                          
         MVI   ERROR,POLTIME                                                    
         B     CHK01NEQ                                                         
*                                                                               
C01E070  GOTOR VDATCON,DMCB,(3,B.BDSTART),(2,BUYDATES)                          
         GOTOR (RF),DMCB,(3,B.BDEND),(2,BUYDATES+2)                             
*                                                                               
CHK01XEQ J     EQEXIT                                                           
CHK01NEQ J     NEQEXIT                                                          
         EJECT                                                                  
***********************************************************************         
*        CHK0B0C  -  CHECKING 0B AND 0C                               *         
**  R6 IS ALREADY POINTING TO THE BEGINNING OF THE 0B/0C                        
***********************************************************************         
REG      USING REGELEM,R6                                                       
CHK0B0C  NTR1  BASE=*,LABEL=*                                                   
         CLI   REG.RCODE,X'0C'     TEST OTO                                     
         BNE   C0B0C030             - NOPE                                      
         TM    REG.RSTATUS,X'80'   TEST MINUS                                   
         BZ    C0B0C030             - NOPE                                      
***  CHECK PREVIOUS ELEMENT                                                     
X        USING REGELEM,RE                                                       
         L     RE,LASTELEM                                                      
         CLI   X.RCODE,X'0B'       IS IT X'0B'?                                 
         BE    C0B0C010             - YUP, GOOD START                           
         CLI   X.RCODE,X'0C'       IS IT X'0C'?                                 
         BNE   CHKOTONO                                                         
C0B0C010 TM    X.RSTATUS,X'40'     SPOT HAS BEEN MINUSED?                       
         BZ    CHKOTONO             - NOPE, NO GOOD                             
*                                                                               
         CLI   REG.RLEN,10         IS IT HIGHER THAN X'0A' LENGTH?              
         BNH   C0B0C020             - NOPE, DON'T NEED MORE CHECKS              
         CLC   X.RDATE,REG.RDATE   COMPARE THE DATES                            
         BNE   CHKOTONO                                                         
         CLC   X.RPPRD(2),REG.RPPRD COMPARE THE PRODUCT AND TIME                
         BNE   CHKOTONO                                                         
***  CHECK PREVIOUS ELEMENT                                                     
***  CHECK NEXT ELEMENT                                                         
C0B0C020 LLC   RE,1(R6)                                                         
         AR    RE,R6                                                            
         CLI   X.RCODE,X'0C'       TEST OTO                                     
         BNE   C0B0C030                                                         
         TM    X.RSTATUS,X'80'     TEST MINUS                                   
         BZ    C0B0C030             - NOPE                                      
         DROP  X                                                                
***  CHECK NEXT ELEMENT                                                         
CHKOTONO MVI   ERROR,OTOERR        PREVIOUS ELEMENT IS NOT A MINUS              
         B     C0B0CX                                                           
*                                                                               
C0B0C030 DS    0H                                                               
         CLC   SAVRDATE,REG.RDATE                                               
         BNH   C0B0C040                                                         
         MVI   ERROR,BADDATES      DATES OUT OF SEQUENCE                        
         B     C0B0CX                                                           
*                                                                               
C0B0C040 DS    0H                                                               
         CLI   REG.RCODE,X'0C'     IS IT X'0C' ELEMENT?                         
         BE    C0B0C050             - YUP, CONTINUE.                            
         CLC   REG.RDATE,BUYDATES  TEST SPOT DATE BEFORE BDSTART                
         BL    OUTSPOT              - YUP                                       
         CLC   REG.RDATE,BUYDATES+2 TEST SPOT DATE AFTER BDEND                  
         BNH   C0B0C050                                                         
OUTSPOT  MVI   ERROR,SPOTOUT                                                    
         B     C0B0CX                                                           
*                                                                               
C0B0C050 OC    REG.RPAY,REG.RPAY                                                
         BZ    C0B0C070                                                         
         TM    B.BUYRCNTL,X'80'    IS THE BUY DELETED?                          
         BZ    C0B0C060                                                         
         MVI   ERROR,PAIDGONE      PAID SPOT IN DELETED RECORD                  
         B     C0B0CX                                                           
                                                                                
C0B0C060 DS    0H                                                               
W        USING REGELEM,WORK                                                     
         XC    WORK(18),WORK       MOVE SPOT                                    
         LA    RE,WORK                                                          
         LLC   RF,REG.RLEN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),REG.REGELEM MOVE SPOT                                    
         AHI   RF,1                RESTORE ELEMENT LENGTH                       
*                                                                               
         MVI   W.RSTATUS,0         CLEAR STATUS BYTE FOR CHKSUM                 
         MVI   W.RPSTAT2,0         CLEAR MAKEGOOD SEQNUM                        
*                                                                               
         SR    R0,R0                                                            
         CKSM  R0,RE                                                            
         JO    *-4                                                              
         AL    R0,SUM              SUM THIS BABY UP                             
         ST    R0,SUM              SAVE IT OFF                                  
*                                                                               
C0B0C070 DS    0H                                                               
         CLI   REG.RLEN,RLPOL1LQ   NON-POL BUY OR UNALLOCATED?                  
         BNL   C0B0C080            - NO                                         
         CLI   SVCPROF1,C'2'       - YES, NOW TEST BRD POL CLIENT?              
         BNE   C0B0CX               - NO                                        
MISSBRD  MVI   ERROR,NOPROD         - ERROR, MISSING BRAND!                     
         B     C0B0CX                                                           
*                                                                               
C0B0C080 CLI   REG.RPPRD,X'FF'     DO WE HAVE AN POL PRODUCT?                   
         BE    MISSBRD             -YES, ERROR!                                 
         CLI   REG.RPPRD,X'0'      -NO, DO WE HAVE ALLOCATED PRODUCT?           
         BE    MISSBRD              - NO, ERROR!                                
*                                                                               
C0B0C090 CLI   SVCPROF1,C'2'       TEST BRD POOL                                
         BNE   C0B0C095            -NO                                          
         CLC   REG.RPPRD,SVBDMSPR  -YES, MATCH ON MASTER BRD                    
         BNE   BADBRD               -NO, ERROR!                                 
*                                                                               
C0B0C095 XR    R1,R1               INIT0                                        
         LR    R2,R1               INIT0                                        
         ICM   R1,1,REG.RPTIME     PRODUCT RPTIME                               
         BZ    ZEROSPLT            -ERROR, CAN'T HAVE 0 SPLIT TIME              
*                                                                               
         CLI   REG.RLEN,RLPOL2LQ   X'12' - DOES IT HAVE A PIGGYBACK             
         BH    BADELEM             -ERROR, BAD ELEMENT                          
         BE    C0B0C100            -YES                                         
*                                                                               
* SPOT DOES NOT HAVE PIGGY                                                      
*                                                                               
         CLI   SVCPROF1,C'2'       -NO, TEST BRD POOL?                          
         BNE   C0B0C110             -NO                                         
         CLI   SVBDMSPR+1,0         -YES, HAVE MASTER PIGGY?                    
         BE    C0B0C110              -NO, THEN WE ARE OK                        
         B     MISSBRD               -YES, ERROR, SPOT IS MISSING PIGGY         
*                                                                               
* SPOT HAS PIGGY                                                                
*                                                                               
C0B0C100 ICM   R2,1,REG.RPTIME+4   PIGGYBACK RPTIME                             
         BZ    ZEROSPLT            -ERROR, CAN'T HAVE 0 SPLIT TIME              
         CLC   REG.RPPRD(1),REG.RPPRD+4 MATCH PRD AND PIGGY?                    
         BE    SAMEPRDS                 - YES, ERROR, SAME PRD & PIGGY          
         CLI   SVCPROF1,C'2'       TEST BRD POOL                                
         BNE   C0B0C110            -NO                                          
         CLC   REG.RPPRD+4(1),SVBDMSPR+1 -YES, MATCH ON MASTER PIGGY            
         BNE   BADBRD               -NO, ERROR, PIGGY DOES NOT MATCH            
C0B0C110 AR    R1,R2                                                            
         CLM   R1,1,SAVBDSEC       DO WE HAVE THE SAME TIME?                    
         BNE   WRONGSEC            -NOPE, ERROR                                 
         B     C0B0CX                                                           
*                                                                               
BADBRD   MVI   ERROR,BADMSPRD      PIGGY DOES NOT MATCH MASTER                  
         B     C0B0CX                                                           
*                                                                               
BADELEM  MVI   ERROR,ELEMERR                                                    
         B     C0B0CX                                                           
*                                                                               
WRONGSEC MVI   ERROR,BADTIME       SUM OF SPLITS NOT EQUAL TO LINE TIME         
         B     C0B0CX                                                           
*                                                                               
SAMEPRDS MVI   ERROR,SAMEBRDS      SAME PRD AND PIG                             
         B     C0B0CX                                                           
*                                                                               
ZEROSPLT MVI   ERROR,PDPBSPL0      CAN'T HAVE 0 SPLIT                           
         B     C0B0CX                                                           
*                                                                               
         DROP  W                                                                
C0B0CX   DS    0H                                                               
         MVC   SAVRDATE,REG.RDATE  SAVE OFF THE RDATE                           
         CLI   ERROR,0             ANY ERROR?                                   
         JE    EXIT                 - NOPE                                      
         STCM  R6,15,SPBYELEM       - NOPE, SAVE THE ADDRESS                    
         J     EXIT                                                             
         DROP  REG                                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        CHKSPILL - CHECK THAT SPILL ELEMS DO NOT SPILL TO HOME MARKET          
*   R6 = A(1ST 03 ELEM)                                                         
*                                                                               
***********************************************************************         
ND       USING NDELEM,R6                                                        
CHKSPILL NTR1  BASE=*,LABEL=*                                                   
         XR    R0,R0                                                            
*                                                                               
CKSPL10  CLI   ND.NDELEM,0         END OF RECORD                                
         BE    CKSPLX                                                           
         CLI   ND.NDELEM,NDCSPLQ   SPILL ELEM                                   
         BNE   CKSPL20                                                          
         CLC   B.BUYKMKT,ND.NDAGYMKT  SAME AS HOME MKT?                         
         BE    CKSPLER                                                          
*                                                                               
CKSPL20  IC    R0,ND.NDLEN                                                      
         AR    R6,R0                                                            
         B     CKSPL10                                                          
*                                                                               
CKSPLER  MVI   ERROR,SPILLMKT                                                   
         J     NEQEXIT                                                          
*                                                                               
CKSPLX   J     EQEXIT                                                           
         DROP  ND                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        CHK68 - CHECK THAT PERCENTAGES OF NTWK BUY ADD UP TO 100               
*   R6 = A(1ST 68 ELEM)                                                         
*                                                                               
***********************************************************************         
CHK68    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    B.BUYKMKT,B.BUYKMKT     ONLY FOR NETWORK LEVEL BUYS!             
         BNZ   CHK68X                                                           
*                                                                               
NTWK     USING NTWKELEM,R6                                                      
         XR    R0,R0                                                            
*                                                                               
CHK68B   CLI   NTWK.NTWKCODE,0      END ?                                       
         BE    CHK68F               YES                                         
         CLI   NTWK.NTWKCODE,NTWKCODQ   NETWK STATION ELEMENT (X'68') ?         
         BNE   CHK68D               NO - NEXT ELEMENT                           
*                                                                               
         CLI   NTWK.NTWKLEN,11      LENGTH OF 11                                
         BL    CHK68D               NO - NO NETWK SHARE                         
*                                                                               
         ICM   R1,15,NTWK.NTWKSHR   % OF NTWK BUY                               
         AR    R0,R1                                                            
*                                                                               
CHK68D   LLC   R1,NTWK.NTWKLEN                                                  
         AR    R6,R1                                                            
         B     CHK68B                                                           
*                                                                               
CHK68F   C     R0,=F'100000'                                                    
         BE    CHK68X                                                           
         MVI   ERROR,BAD68                                                      
         J     NEQEXIT                                                          
*                                                                               
CHK68X   J     EQEXIT                                                           
         DROP  NTWK                                                             
         DROP  B                                                                
         EJECT                                                                  
*                                                                               
WORKD    DSECT                     ** SPDEMUP GLOBAL W/S **                     
DUB      DS    D                                                                
FULL     DS    F                                                                
SUM      DS    F                                                                
WORK     DS    XL64                                                             
DMCB     DS    6F                                                               
VCOMFACS DS    V                                                                
VDATCON  DS    V                                                                
VMSUNPK  DS    V                                                                
AERRMSG  DS    A                   ADDRESS FOR ONLINE APP ERROR MESSAGE         
APARM    DS    A                   A(USER PARAMETER LIST)                       
SAVERD   DS    A                   SAVED RD VALUE FOR ERROR EXIT                
LASTELEM DS    A                   THE PREVIOUS ELEMENT ADDRESS                 
BUYDATES DS    XL4                 START AND END DATES                          
SAVRDATE DS    H                   SAVE THE OLD RDATE                           
SAVBDSEC DS    XL1                 SAVE THE BDSEC                               
SVBDCOST DS    XL3                                                              
SAVEELEM DS    XL1                 SAVE THE LAST ELEMENT CODE                   
SVCPROF1 DS    C                   SAVE CPROF FIELD 1 (BRAND/POL TRNDS)         
SVBDMSPR DS    XL2                 SAVE BDMASPRD                                
ERROR    DS    XL1                 1 BYTE ERROR FIELD                           
                                                                                
BYTE     DS    XL1                                                              
MISCFLG1 DS    XL1                 MISCELLANEOUS FLAG 1                         
HAVE22   EQU   X'80'                - WE HAVE A X'22' ELEMENT                   
HAVE23   EQU   X'40'                - WE HAVE A X'23' ELEMENT                   
SHOWERR  EQU   X'01'                - WE ARE SHOWING THE ERROR MESSAGE          
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
       ++INCLUDE SPBUYVALD                                                      
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009SPBUYVAL  01/11/18'                                      
         END                                                                    
